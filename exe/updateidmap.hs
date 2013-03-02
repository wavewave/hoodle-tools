{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (threadDelay)
import           Control.Lens
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Either 
import           Data.Attoparsec 
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import           Data.Digest.Pure.MD5
import Data.IORef
import qualified Data.Map as M
import Data.String 
import Data.Time.Clock 
import           Data.Time.Clock.POSIX
import Filesystem.Path 
import           System.Directory 
import qualified System.FilePath as FP
import           System.FSNotify 
import           System.IO
import           System.Posix.Files 
import           System.Process
-- 
import Data.Hoodle.Simple
import Text.Hoodle.Parse.Attoparsec 
-- 


dtime_bound :: NominalDiffTime 
dtime_bound = realToFrac (picosecondsToDiffTime (floor 1e13))

splitfunc :: String -> (String,(String,String))
splitfunc str = 
  let (str1,rest1) = break (==' ') str 
      (str2,rest2) = break (==' ') (tail rest1)
      str3 = read (tail rest2)
  in (str1,(str2,str3))

getFileIDList :: EitherT String IO (M.Map String (String,FP.FilePath))
getFileIDList = do 
  homedir <- liftIO $ getHomeDirectory 
  let fp = homedir FP.</> "Dropbox" FP.</> "hoodleiddb.dat"
  b <- liftIO $ doesFileExist fp 
  if b 
    then do 
      str <- liftIO $ readFile fp 
      let ls = lines str 
          alist = map splitfunc ls
          pathmap = M.fromList alist
      return pathmap
    else 
      fail "no id map" -- eturn Nothing 

-- | using attoparsec without any built-in xml support 
checkVersionAndGetIfHigherVersion :: B.ByteString -> EitherT String IO  Hoodle 
checkVersionAndGetIfHigherVersion bstr = do 
  v <- hoistEither (parseOnly checkHoodleVersion bstr) 
  if ( v < "0.1.9999" ) 
    then left "low version"
    else hoistEither (parseOnly hoodle bstr)


-- |  
getIDMD5 ::  FP.FilePath -> EitherT String IO (String,String)
getIDMD5 fp  = do 
  liftIO $ putStrLn $ fp ++ " is now being analyzed " 
  bstr <- liftIO $ B.readFile fp
  h <- checkVersionAndGetIfHigherVersion bstr
  fs <- liftIO $ getFileStatus fp 
  let etime = modificationTime fs
      utctime = posixSecondsToUTCTime (realToFrac etime)
  let idstr = B.unpack (view hoodleID h)
      md5str = show (md5 (L.fromChunks [bstr]))
      --     nfilename = "UUID_"++idstr++"_MD5Digest_"++md5str++"_ModTime_"++(show utctime) ++ ".hdl"
      -- B.writeFile nfilename bstr 
  return (idstr,md5str)
  --    hPutStrLn fh ( idstr ++  " " ++ md5str ++ " " ++ show fp )



action :: IORef UTCTime -> Event -> IO () 
action tref ev = do
  e <- runEitherT $ do 
    otime <- liftIO $ readIORef tref 
    ctime <- liftIO $ getCurrentTime
    let dtime = diffUTCTime ctime otime 
    when (dtime <= dtime_bound) $ left "too early"
    liftIO (threadDelay 1000000)
    liftIO (print ev)
    homedir <- liftIO $ getHomeDirectory
    r <- liftIO $ readProcess "find" [homedir FP.</> "Dropbox" FP.</> "hoodle","-name","*.hdl","-mmin","-1","-print"] "" 
    let nfilelst = lines r 
    pathmap <- getFileIDList
    liftIO $ print pathmap 
    idmd5lst <- mapM getIDMD5 nfilelst
    let fileidmd5lst =  zip nfilelst idmd5lst 
    let npathmap = foldr (\(f,(i,s)) m -> M.adjust (const (s,f)) i m) pathmap fileidmd5lst 
    -- mapM_ (liftIO . print) (M.toList npathmap)
    let dbfile = homedir FP.</> "Dropbox" FP.</> "hoodleiddb.dat"
    liftIO $ withFile  dbfile WriteMode $ \h -> do 
      mapM_ (\(i,(s,f))->hPutStrLn h (i ++ " " ++ s ++ " " ++ show f)) (M.toList npathmap)
  case e of 
    Left err -> putStrLn err 
    Right () -> return ()
{-
      -- print pathmap 
      case mpathmap of 
        Nothing -> return () 
        Just pathmap -> do 
          -- system "idfilepathdb" 
          writeIORef tref ctime 

-}



main :: IO () 
main = do 
  ctime <- getCurrentTime 
  tref <- newIORef ctime 

  putStrLn "fsnotify test"
  homedir <- getHomeDirectory
  let wd = fromString homedir </> "Dropbox" </> "hoodle"

  man <- startManager
  watchTree man wd (const True) (action tref) -- (\x -> threadDelay 1000000 >> print x )
  print "press return to stop"
  getLine 
  print "watching stopped, press return to exit"
  stopManager man 
  getLine 
  return ()
