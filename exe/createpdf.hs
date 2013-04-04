{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Applicative 
import Control.Concurrent 
import Control.Lens (_1,_2,_3,_4,view,at )
import Control.Monad
import Control.Monad.Loops
import Control.Monad.Trans
import Control.Monad.Trans.State 
import Data.Attoparsec.Char8
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BSL
import Data.Int
import Data.List 
import Data.UUID.V4
import Network.HTTP.Base
import System.Directory
import System.FilePath 
import System.Environment
import System.IO
import qualified System.IO.Streams as Streams
-- 
import Pdf.Toolbox.Core
import Pdf.Toolbox.Document
import Pdf.Toolbox.Document.Internal.Types 
-- 
import qualified Data.Hoodle.Simple as S
import Graphics.Hoodle.Render 
import Hoodle.Coroutine.File 
import Text.Hoodle.Parse.Attoparsec 


import System.Posix.Files
-- import System.Posix.Files.ByteString 
import System.Posix.IO
import System.Posix.Process 


data Annot = Annot { annot_rect :: (Int, Int, Int, Int) 
                   , annot_border :: (Int ,Int, Int) 
                   , annot_url :: String  
                   } 

data AppState = AppState {
  stNextFree :: Int,
  stPageRefs :: [Ref],
  stRootNode :: Ref
  }

initialAppState :: AppState
initialAppState = AppState {
  stNextFree = 1,
  stPageRefs = [],
  stRootNode = error "stRootNode"
  }

nextFreeIndex :: Monad m => StateT AppState m Int
nextFreeIndex = do
  st <- get
  let index = stNextFree st
  put $ st {stNextFree = index + 1}
  return index

putPageRef :: Monad m => Ref -> StateT AppState m ()
putPageRef ref =
  modify $ \st -> st {stPageRefs = ref : stPageRefs st}

writeTrailer :: StateT AppState (PdfWriter IO) ()
writeTrailer = do
  pageRefs <- gets stPageRefs

  rootRef <- gets stRootNode
  lift $ writeObject rootRef $ ODict $ Dict [
    ("Type", OName "Pages"),
    ("Count", ONumber $ NumInt $ length pageRefs),
    ("Kids", OArray $ Array $ map ORef $ reverse pageRefs)
    ]

  catalogIndex <- nextFreeIndex
  let catalogRef = Ref catalogIndex 0
  lift $ writeObject catalogRef $ ODict $ Dict [("Type", OName "Catalog"), ("Pages", ORef rootRef)]

  count <- gets stNextFree
  lift $ writeXRefTable 0 (Dict [("Size", ONumber $ NumInt $ count - 1), ("Root", ORef catalogRef)])

writeObjectChildren :: Object () -> Pdf (StateT AppState (PdfWriter IO)) (Object ())
writeObjectChildren (ORef r) = do
  o <- lookupObject r
  case o of
    OStream s -> do
      ref <- writeStream s
      return $ ORef ref
    _ -> do
      let o' = mapObject (error "impossible") o
      o'' <- writeObjectChildren o'
      index <- (lift.lift) nextFreeIndex
      let ref = Ref index 0
      (lift.lift.lift) $ writeObject ref $ mapObject (error "impossible") o''
      return $ ORef ref
writeObjectChildren (ODict (Dict vals)) = do
  vals' <- forM vals $ \(key, val) -> do
    val' <- writeObjectChildren val
    return (key, val')
  return $ ODict $ Dict vals'
writeObjectChildren (OArray (Array vals)) = do
  vals' <- forM vals writeObjectChildren
  return $ OArray $ Array vals'
writeObjectChildren o = return o


writeStream :: Stream Int64 -> Pdf (StateT AppState (PdfWriter IO)) Ref
writeStream s@(Stream dict _) = do
    len <- lookupDict "Length" dict >>= deref >>= fromObject >>= intValue
    ris <- getRIS
    Stream _ is <- rawStreamContent ris len s
    content <- liftIO $ BSL.fromChunks `liftM` Streams.toList is
    index <- (lift . lift) nextFreeIndex
    let ref = Ref index 0
    dict' <- writeObjectChildren (ODict dict) >>= fromObject
    lift . lift . lift $ writeObject ref $ OStream $ Stream dict' content
    return ref


writeAnnot :: Annot -> Pdf (StateT AppState (PdfWriter IO)) Ref
writeAnnot Annot{..} = do  
    annotIndex <- (lift.lift) nextFreeIndex
    actionIndex <- (lift.lift) nextFreeIndex
    let annotRef = Ref annotIndex 0 
        actionRef = Ref actionIndex 0 
    let annotDict = Dict [ ("Type", OName "Annot") 
                         , ("Subtype", OName "Link") 
                         , ("Rect", OArray $ Array [ ONumber (NumInt (view _1 annot_rect))
                                                   , ONumber (NumInt (view _2 annot_rect))
                                                   , ONumber (NumInt (view _3 annot_rect))
                                                   , ONumber (NumInt (view _4 annot_rect)) ] ) 
                         , ("Border", OArray $ Array [ ONumber (NumInt (view _1 annot_border))
                                                     , ONumber (NumInt (view _2 annot_border))
                                                     , ONumber (NumInt (view _3 annot_border)) ] ) 
                         , ("A", ORef actionRef) 
                         ] 
        actionDict = Dict [ ("S", OName "URI" ) 
                          , ("URI", OStr (Str (B.pack annot_url)))
                          ] 
    lift.lift.lift $ writeObject annotRef $ ODict annotDict 
    lift.lift.lift $ writeObject actionRef $ ODict actionDict 
    return annotRef 


writePdfPageWithAnnot :: Maybe [Annot] -> Page -> Pdf (StateT AppState (PdfWriter IO)) ()
writePdfPageWithAnnot mannots page@(Page _ pageDict) = do
  parentRef <- lift.lift $ gets stRootNode
  pageIndex <- (lift.lift) nextFreeIndex
  let pageRef = Ref pageIndex 0
  lift.lift $ putPageRef pageRef
  contentRefs <- pageContents page
  contentRefs' <- forM contentRefs $ \r -> do
    s <- lookupObject r >>= toStream
    writeStream s
  resources <- lookupDict "Resources" pageDict >>= deref >>= writeObjectChildren
  
  case mannots of 
    Nothing -> lift.lift.lift $ writeObject pageRef $ ODict 
                 $ Dict [ ("Type", OName "Page")
                        , ("Contents", OArray $ Array $ map ORef contentRefs')
                        , ("Resources", resources)
                        , ("Parent", ORef parentRef)
                        ]
    Just anns -> do
      annrefs <- mapM writeAnnot anns
      lift.lift.lift $ writeObject pageRef $ ODict 
                 $ Dict [ ("Type", OName "Page")
                        , ("Contents", OArray $ Array $ map ORef contentRefs')
                        , ("Resources", resources)
                        , ("Parent", ORef parentRef)
                        , ("Annots", (OArray . Array . map ORef) annrefs) 
                        ]


makeAnnot :: S.Dimension -> S.Link -> Annot
makeAnnot (S.Dim pw ph) lnk = 
  let (x,y) = S.link_pos lnk
      S.Dim w h = S.link_dim lnk
      pwi = floor pw 
      phi = floor ph
      xi = floor x
      yi = floor y 
      wi = floor w 
      hi = floor h
      fp = (B.unpack . S.link_location) lnk 
      (dir,fn) = splitFileName fp 
      (fb,ext) = splitExtension fn 
  in Annot { annot_rect = (xi,phi-yi,xi+wi,phi-(yi+hi))
           , annot_border = (16,16,1) 
           , annot_url = "file://" ++ dir </> urlEncode fb <.> "hdl"
             -- "file:///home/wavewave/repo/gist/createlink/2013-04-02%2017:20:10.77225%20UTC.hdl" 
       }




writePdfFile :: FilePath 
             -> [(Int,[S.Link])]
             -> StateT AppState (PdfWriter IO) ()
writePdfFile path nlnks = do
  handle <- liftIO $ openBinaryFile path ReadMode
  res <- runPdfWithHandle handle knownFilters $ do
    encrypted <- isEncrypted
    when encrypted $ setUserPassword defaultUserPassord
    root <- document >>= documentCatalog >>= catalogPageNode
    count <- pageNodeNKids root
    forM_ [0..count-1] $ \i -> do
      page <- pageNodePageByNum root i
      let dim = S.Dim 612.0 792.0 
      let mannots = fmap (map (makeAnnot dim)) (lookup (i+1) nlnks) 
      writePdfPageWithAnnot mannots page
  when (isLeft res) $ error $ show res
  liftIO $ hClose handle

getLinks :: S.Page -> [S.Link]
getLinks pg = do 
  l <- view S.layers pg 
  S.ItemLink lnk <- view S.items l
  return lnk 



main :: IO ()
main = do
  args <- getArgs 
  let fn = args !! 0 

  bstr <- B.readFile fn 
  case parseOnly hoodle bstr of 
    Left str -> error str 
    Right hdl -> do
      let npgs = zip [1..] (view S.pages hdl)
          npglnks = map ((,) <$> fst <*> getLinks . snd) npgs  
      rhdl <- cnstrctRHoodle hdl 
      let fp = "testtemp.pdf"
      renderjob rhdl fp
      runPdfWriter Streams.stdout $ do 
        writePdfHeader
        deleteObject (Ref 0 65535) 0 
        flip evalStateT initialAppState $ do 
         index <- nextFreeIndex 
         modify $ \st -> st { stRootNode = Ref index 0} 
         writePdfFile fp npglnks
         writeTrailer


{-
-- | 
checkPipe :: FilePath -> IO ()
checkPipe fp = untilM_ (threadDelay 10000) (fileExist fp) 

-- | 
mkTmpFileName :: IO FilePath 
mkTmpFileName = do
  tdir <- getTemporaryDirectory 
  tuuid <- nextRandom
  return $ tdir </> show tuuid <.> "fifo"

-- | 
existThenRemove :: FilePath -> IO () 
existThenRemove fp = fileExist fp >>= \b -> when b (removeLink fp) 

-- |
pipeAction :: IO () -> (B.ByteString -> IO a) -> IO a 
pipeAction sender receiver = pipeActionWith sender (receiver <=< B.readFile)

-- |
pipeActionWith :: IO () -> (FilePath -> IO a) -> IO a 
pipeActionWith sender receiverf = do 
  filename <- mkTmpFileName 
  existThenRemove filename 
  createNamedPipe filename (unionFileModes ownerReadMode ownerWriteMode)
  forkProcess $ do  
    fd <- openFd filename WriteOnly Nothing defaultFileFlags
    dupTo fd stdOutput 
    closeFd fd
    sender 
    hFlush stdout 
  r <- checkPipe filename >> receiverf filename  
  removeLink filename  
  return r 
-}

