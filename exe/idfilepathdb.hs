{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
-- 
-- testing program for attoparsec and sax hoodle parser
-- 
import           Control.Applicative ((<$>))
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.Maybe 
import           Data.Attoparsec 
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import           Data.Maybe (mapMaybe)
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Data.Digest.Pure.MD5
import           System.Directory
import           System.FilePath
import           System.FilePath.Posix
import           System.Environment 
import           System.IO
import           System.Posix.Files 
import           System.Process

import Data.Data
import System.Console.CmdArgs
-- 
import Data.Hoodle.Simple
-- 
import Text.Hoodle.Parse.Attoparsec 
-- import qualified Text.Hoodle.Migrate.V0_1_999_to_V0_1_9999 as MV
-- import Text.Hoodle.Parse.Conduit 
import Graphics.Hoodle.Render
import Graphics.Hoodle.Render.Type
import Graphics.Rendering.Cairo 

data IdFilePathDB = AllFiles { hoodlehome :: FilePath }
                  | SingleFile { hoodlehome :: FilePath 
                               , singlefilename :: FilePath } 
                  deriving (Show,Data,Typeable)

allfiles :: IdFilePathDB 
allfiles = 
  AllFiles { hoodlehome = def &= typ "HOODLEHOME" &= argPos 0 } 

singlefile :: IdFilePathDB 
singlefile = 
  SingleFile { hoodlehome = def &= typ "HOODLEHOME" &= argPos 0 
             , singlefilename = def &= typ "FILEPATH" &= argPos 1
             }

mode :: IdFilePathDB
mode = modes [allfiles, singlefile] 

main :: IO () 
main = do 
  params <- cmdArgs mode
  case params of 
    AllFiles hdir -> allfilework hdir 
    SingleFile hdir fp -> singlefilework hdir fp 
  
  
allfilework hdir = do 
  homedir <- getHomeDirectory 
  r <- readProcess "find" [homedir </> "Dropbox" </> "hoodle","-name","*.hdl","-print"] "" 
  mapM_ (singlefilework hdir) (lines r)


splitfunc :: String -> (String,(String,String))
splitfunc str = 
  let (str1,rest1) = break (==' ') str 
      (str2,rest2) = break (==' ') (tail rest1)
      str3 = read (tail rest2)
  in (str1,(str2,str3))

singlefilework hdir oldfp = do 
  homedir <- getHomeDirectory 
  tmpdir <- getTemporaryDirectory 
  -- r <- readProcess "find" [hdir,"-name","*.hdl","-print"] "" 
  let origdbfile = homedir </> "Dropbox" </> "hoodleiddb.dat"
      tmpfile = tmpdir </> "hoodleiddb.dat"
  copyFile origdbfile tmpfile 
  
  fp <- makeRelative hdir <$> canonicalizePath oldfp 
  str <- readFile tmpfile 
  let assoclst = (map splitfunc . lines) str 
      assocmap = M.fromList assoclst 
  
  let replacefunc n _ = Just n 

  muuid <- checkHoodleIdMd5 oldfp 
  let nmap = case muuid of 
               Nothing -> assocmap 
               Just (uuid,md5str) -> M.alter (replacefunc (md5str,fp)) uuid assocmap
      nstr = (unlines . map (\(x,(y,z))->x ++ " " ++ y ++ " " ++ show z) . M.toList) nmap 
 
  writeFile origdbfile nstr 
  removeFile tmpfile 


checkHoodleIdMd5 :: FilePath -> IO (Maybe (String,String))
checkHoodleIdMd5 fp = do 
  bstr <- B.readFile fp
  eh <- checkVersionAndGetIfHigherVersion bstr
  case eh of 
    Left str -> print str >> return Nothing 
    Right h -> do
      let idstr = B.unpack (view hoodleID h)
          md5str = show (md5 (L.fromChunks [bstr]))
      return (Just (idstr,md5str))
      
{- 
-- |  
onefile :: Handle -> FilePath -> IO ()   
onefile fh fp  = do 
  putStrLn $ fp ++ " is now being analyzed " 
  muuidmd5 <- checkHoodleIdMd5 fp 
  {- bstr <- B.readFile fp
  eh <- checkVersionAndGetIfHigherVersion bstr
  case eh of 
    Left str -> print str -}
  case muuidmd5 of 
    Nothing -> return () 
    Just (idstr,md5str) -> do 
      fs <- getFileStatus fp 
      let etime = modificationTime fs
          utctime = posixSecondsToUTCTime (realToFrac etime)
      hPutStrLn fh ( idstr ++  " " ++ md5str ++ " " ++ show fp )
  -}    

     
-- | using attoparsec without any built-in xml support 
checkVersionAndGetIfHigherVersion :: B.ByteString -> IO (Either String Hoodle) 
checkVersionAndGetIfHigherVersion bstr = do 
  case parseOnly checkHoodleVersion bstr of 
    Left str -> return (Left str )
    Right v -> do 
      if ( v < "0.1.9999" ) 
        then return (Left "low version") -- MV.migrate bstr
        else return (parseOnly hoodle bstr)

   
