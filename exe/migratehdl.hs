{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Blaze.ByteString.Builder 
import Control.Applicative 
import Control.Monad
import Data.Attoparsec.Char8
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe
import System.Directory
import System.Directory.Tree 
import System.FilePath 
import System.Environment
import System.Process
-- 
import Text.Hoodle.Parse.Attoparsec 
import qualified Text.Hoodle.Migrate.V0_1_1_to_V0_2 as MV
import Text.Hoodle.Builder 
-- 
-- import Debug.Trace

isFile :: DirTree a -> Bool
isFile (File _ _) = True
isFile _ = False

takeFile :: DirTree a -> Maybe a
takeFile x | isFile x = (Just . file) x 
takeFile _ | otherwise = Nothing 


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


isHdl :: FilePath -> Bool
isHdl = ( == ".hdl") <$> takeExtension 

isOldVersion :: FilePath -> IO Bool 
isOldVersion fp = do 
  bstr <- B.readFile fp 
  case parseOnly checkHoodleVersion bstr of 
    Left _ -> return False
    Right v -> do 
      if ( v < "0.1.999" ) 
        then return True 
        else return False

main :: IO ()
main = do
  -- initGUI 
  args <- getArgs 
  let rootpath = args !! 0
      backuppath = args !! 1 

  (_r :/ r') <- build rootpath 
  let files = catMaybes . map takeFile . flattenDir $ r' 
      hdlfiles = filter isHdl files 
      pairs = map ((,) <$> id
                   <*> (backuppath </>) . makeRelative rootpath ) hdlfiles 
  oldversionpairs <- filterM (isOldVersion . fst) pairs 
  mapM_ print oldversionpairs 
  mapM_ createBackup oldversionpairs 
  mapM_ migrateVersion oldversionpairs 

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

