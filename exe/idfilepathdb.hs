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
import           Data.Maybe (mapMaybe, isNothing, fromJust)
-- import           Data.Foldable 
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
import Graphics.Hoodle.Render
import Graphics.Hoodle.Render.Type
import Graphics.Rendering.Cairo 
-- 




data IdFilePathDB = AllFiles { hoodlehome :: FilePath }
                  | SingleFile { hoodlehome :: FilePath 
                               , singlefilename :: FilePath } 
                  | DBDiff
                  deriving (Show,Data,Typeable)

allfiles :: IdFilePathDB 
allfiles = 
  AllFiles { hoodlehome = def &= typ "HOODLEHOME" &= argPos 0 } 

singlefile :: IdFilePathDB 
singlefile = 
  SingleFile { hoodlehome = def &= typ "HOODLEHOME" &= argPos 0 
             , singlefilename = def &= typ "FILEPATH" &= argPos 1
             }
  
dbdiff :: IdFilePathDB 
dbdiff = DBDiff
  
mode :: IdFilePathDB
mode = modes [allfiles, singlefile, dbdiff] 

main :: IO () 
main = do 
  params <- cmdArgs mode
  case params of 
    AllFiles hdir      -> allfilework hdir 
    SingleFile hdir fp -> singlefilework hdir fp 
    DBDiff             -> dbdiffwork
  
allfilework :: FilePath -> IO ()
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


dbdiffwork :: IO ()
dbdiffwork = do 
  homedir <- getHomeDirectory 
  tmpdir <- getTemporaryDirectory 
  let newdbfile = homedir </> "Dropbox" </> "hoodleiddb.dat"
      olddbfile = homedir </> "Dropbox" </> "hoodleiddb.dat.old"
   
  newdbstr <- readFile newdbfile 
  olddbstr <- readFile olddbfile
  let makedb = M.fromList . map splitfunc . lines 
      (newdb,olddb) = (makedb newdbstr, makedb olddbstr) 
     
  mapM_ print (M.toList (checkdiff olddb newdb))     
  -- print newdb 
  -- print olddb 
  
  
data DiffType = Same 
              | RightNew (String,FilePath) 
              | LeftNew (String,FilePath) 
              | Conflict (String,FilePath) (String,FilePath)  
              deriving (Show,Eq) 

checkdiff :: M.Map String (String,FilePath) 
          -> M.Map String (String,FilePath) 
          -> M.Map String DiffType
checkdiff olddb newdb = 
  let oks = M.keys olddb 
      nks = M.keys newdb
      ks = oks ++ nks
      rightnew k = (k,(RightNew . fromJust) (M.lookup k newdb))
      compf k = case (M.lookup k olddb,M.lookup k newdb) of  
        (Nothing,Nothing) -> Nothing
        (Just v,Nothing) -> Just (k,LeftNew v)
        (Nothing,Just v) -> Just (k,RightNew v)
        (Just v,Just v') -> 
          if v == v' then Just (k,Same) else Just (k,Conflict v v')
  in (M.fromList . filter ((/=Same).snd) . mapMaybe compf) ks

singlefilework :: FilePath -> FilePath -> IO ()
singlefilework hdir oldfp = do 
  homedir <- getHomeDirectory 
  tmpdir <- getTemporaryDirectory 
  let origdbfile = homedir </> "Dropbox" </> "hoodleiddb.dat"
      tmpfile = tmpdir </> "hoodleiddb.dat"
  copyFile origdbfile tmpfile 
  fp <- makeRelative hdir <$> canonicalizePath oldfp 
  str <- readFile tmpfile 
  let assoclst = (map splitfunc . lines) str 
      assocmap = M.fromList assoclst 
      replacefunc n _ = Just n 
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
      

-- | using attoparsec without any built-in xml support 
checkVersionAndGetIfHigherVersion :: B.ByteString -> IO (Either String Hoodle) 
checkVersionAndGetIfHigherVersion bstr = do 
  case parseOnly checkHoodleVersion bstr of 
    Left str -> return (Left str )
    Right v -> do 
      if ( v < "0.1.9999" ) 
        then return (Left "low version") 
        else return (parseOnly hoodle bstr)

   
