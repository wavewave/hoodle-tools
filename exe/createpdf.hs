{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative 
import Control.Concurrent 
import Control.Exception
import Control.Lens (_1,_2,_3,_4,view,at )
import Control.Monad
import Control.Monad.Loops
import Control.Monad.Trans
import Control.Monad.Trans.Maybe 
import Control.Monad.Trans.State 
import Data.Attoparsec.Char8
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BSL
import Data.Int
import Data.List 
import Data.UUID.V4
import Graphics.UI.Gtk (initGUI)
import Network.HTTP.Base
import Pdf.Toolbox.Core
import Pdf.Toolbox.Document
import Pdf.Toolbox.Document.Internal.Types 
import System.Directory
import System.Directory.Tree 
import System.FilePath 
import System.Environment
import System.IO
import qualified System.IO.Streams as Streams
-- import System.Posix.Files
-- import System.Posix.IO
-- import System.Posix.Process 
import System.Process
-- 
import qualified Data.Hoodle.Simple as S
import Graphics.Hoodle.Render 
import Hoodle.Coroutine.File 
import Text.Hoodle.Parse.Attoparsec 


-- 
import Debug.Trace


isFile (File _ _) = True
isFile _ = False

takeFile x | isFile x = (Just . file) x 
takeFile x | otherwise = Nothing 




-- | Get the relative url to the site root, for a given (absolute) url
toSiteRoot :: String -> String
toSiteRoot = emptyException . joinPath . map parent
           . filter relevant . splitPath . takeDirectory
  where
    parent            = const ".."
    emptyException [] = "."
    emptyException x  = x
    relevant "."      = False
    relevant "/"      = False
    relevant _        = True


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

-- | 
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

-- |
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

-- | 
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

-- | 
makeAnnot :: S.Dimension -> String -> (FilePath,FilePath) -> S.Link -> IO (Maybe Annot)
makeAnnot (S.Dim pw ph) urlbase (rootpath,currpath) lnk = do 
  let (x,y) = S.link_pos lnk
      S.Dim w h = S.link_dim lnk
      pwi = floor pw 
      phi = floor ph
      xi = floor x
      yi = floor y 
      wi = floor w 
      hi = floor h
      linkpath = (B.unpack . S.link_location) lnk
  b <- doesFileExist linkpath 
  if b 
    then do
      fp <- canonicalizePath linkpath 
      let (dir,fn) = splitFileName fp
          -- rdir = (toSiteRoot . makeRelative rootpath) currpath </> makeRelative rootpath dir  
          rdir = makeRelative rootpath dir 
          (fb,ext) = splitExtension fn 
      return (Just Annot { annot_rect = (xi,phi-yi,xi+wi,phi-(yi+hi))
                         , annot_border = (16,16,1) 
                         , annot_url = urlbase </> rdir </> urlEncode fb <.> "pdf"
                         })
    else return Nothing 


-- | 
writePdfFile :: String -- ^ url base 
             -> (FilePath,FilePath)   -- ^ (root path, curr path)
             -> FilePath    -- ^ pdf file 
             -> [(Int,[S.Link])]
             -> StateT AppState (PdfWriter IO) ()
writePdfFile urlbase (rootpath,currpath) path nlnks = do
  handle <- liftIO $ openBinaryFile path ReadMode
  res <- runPdfWithHandle handle knownFilters $ do
    encrypted <- isEncrypted
    when encrypted $ setUserPassword defaultUserPassord
    root <- document >>= documentCatalog >>= catalogPageNode
    count <- pageNodeNKids root
    forM_ [0..count-1] $ \i -> do
      page <- pageNodePageByNum root i
      let dim = S.Dim 612.0 792.0 
      mannots <- runMaybeT $ do 
                   lnks <- MaybeT . return $ lookup (i+1) nlnks
                   liftM catMaybes . mapM (liftIO . makeAnnot dim urlbase (rootpath,currpath)) $ lnks 
      writePdfPageWithAnnot mannots page
  when (isLeft res) $ error $ show res
  liftIO $ hClose handle

getLinks :: S.Page -> [S.Link]
getLinks pg = do 
  l <- view S.layers pg 
  S.ItemLink lnk <- view S.items l
  return lnk 

isHdl = ( == ".hdl") <$> takeExtension 


main :: IO ()
main = do
  initGUI 
  args <- getArgs 
  let urlbase = args !! 0
      rootpath = args !! 1
      buildpath = args !! 2 
      -- fn = args !! 1 
  (r :/ r') <- build rootpath 
  let files = catMaybes . map takeFile . flattenDir $ r' 
      hdlfiles = filter isHdl files 
      pairs = map ((,) <$> id
                   <*> (buildpath </>) . flip replaceExtension "pdf" . makeRelative rootpath ) hdlfiles 
  updatedpairs <- filterM isUpdated pairs 
  mapM_ (createPdf urlbase rootpath) updatedpairs

isUpdated :: (FilePath,FilePath) -> IO Bool 
isUpdated (ofp,nfp) = do 
  b <- doesFileExist nfp
  if not b 
    then return True
    else do 
      otime <- getModificationTime ofp
      ntime <- getModificationTime nfp 
      return (otime > ntime)
  



createPdf :: String -> FilePath -> (FilePath,FilePath) -> IO ()
createPdf urlbase rootpath (fn,ofn) = catch action (\(e :: SomeException) -> print e)
  where 
    action = do 
      putStrLn fn 
      let (odir,_) = splitFileName ofn 
      b <- doesDirectoryExist odir
      when (not b) $ system ("mkdir -p " ++ odir) >> return () 
      let (currpath,_) = splitFileName fn
      Streams.withFileAsOutput ofn $ \ostr -> do 
        bstr <- B.readFile fn 
        case parseOnly hoodle bstr of 
          Left str -> error str 
          Right hdl -> do
            let npgs = zip [1..] (view S.pages hdl)
                npglnks = map ((,) <$> fst <*> getLinks . snd) npgs  
            rhdl <- cnstrctRHoodle hdl 
            tempfile <- (</>) <$> getTemporaryDirectory <*> liftM show nextRandom
            renderjob rhdl tempfile
            runPdfWriter ostr $ do 
              writePdfHeader
              deleteObject (Ref 0 65535) 0 
              flip evalStateT initialAppState $ do 
                index <- nextFreeIndex 
                modify $ \st -> st { stRootNode = Ref index 0} 
                writePdfFile urlbase (rootpath,currpath) tempfile npglnks
                writeTrailer
            removeFile tempfile 


