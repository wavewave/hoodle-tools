{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Blaze.ByteString.Builder 
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
import qualified Text.Hoodle.Migrate.V0_1_1_to_V0_2 as MV
import Text.Hoodle.Builder 
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



isHdl = ( == ".hdl") <$> takeExtension 

isOldVersion :: FilePath -> IO Bool 
isOldVersion fp = do 
  bstr <- B.readFile fp 
  case parseOnly checkHoodleVersion bstr of 
    Left str -> return False
    Right v -> do 
      if ( v < "0.1.999" ) 
        then return True 
        else return False



main :: IO ()
main = do
  initGUI 
  args <- getArgs 
  let rootpath = args !! 0
      backuppath = args !! 1 

  (r :/ r') <- build rootpath 
  let files = catMaybes . map takeFile . flattenDir $ r' 
      hdlfiles = filter isHdl files 
      pairs = map ((,) <$> id
                   <*> (backuppath </>) . makeRelative rootpath ) hdlfiles 
  oldversionpairs <- filterM (isOldVersion . fst) pairs 
  mapM_ print oldversionpairs 
  mapM_ createBackup oldversionpairs 
  mapM_ migrateVersion oldversionpairs 
  -- updatedpairs <- filterM isUpdated pairs 
  -- mapM_ (createPdf urlbase rootpath) updatedpairs

createBackup :: (FilePath,FilePath) -> IO ()
createBackup (ofp,nfp) = do 
    let (ndir,_) = splitFileName nfp 
    b <- doesDirectoryExist ndir
    when (not b) $ system ("mkdir -p " ++ ndir) >> return () 
    putStrLn $ "copy " ++ show ofp ++ " to " ++ show nfp
    copyFile ofp nfp 

migrateVersion :: (FilePath,FilePath) -> IO () 
migrateVersion (ofp,nfp) = do 
    putStrLn $ "doing migration for " ++ ofp 
    getLine 
    bstr <- B.readFile nfp 
    r <- MV.migrate bstr 
    case r of 
      Left err -> error err 
      Right hdl -> BSL.writeFile ofp (toLazyByteString (buildHoodle hdl))



{-
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

-}
