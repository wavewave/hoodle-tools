{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import           Control.Lens
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Either 
import Control.Monad.Trans.Resource
import           Data.Attoparsec 
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Conduit (($$))
import           Data.Conduit.Binary (sourceFile,sourceLbs,sinkFile)
import           Data.Conduit.List (consume)
import           Data.Digest.Pure.MD5
import Data.IORef
import           Data.List (intercalate)
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
      -- tempdir <- liftIO $ getTemporaryDirectory
      -- let tempiddb = tempdir FP.</> "hoodleiddb.dat"
      -- liftIO $ copyFile fp tempiddb
      -- str <- liftIO $ readFile tempiddb
      bstr <- liftIO $ runResourceT (sourceFile fp $$ consume)
      -- h <- liftIO $ openFile fp ReadMode 
      -- bstr <- liftIO $ B.hGetContents h 
      -- liftIO $ hClose h 
      liftIO $ putStrLn $ " length = " ++ show (length bstr)
      let str = (L.unpack . L.fromChunks) bstr 
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



action :: MVar UTCTime -> Event -> IO () 
action tref ev = do
  e <- runEitherT $ do 
    otime <- liftIO $ takeMVar tref 
    ctime <- liftIO $ getCurrentTime
    let dtime = diffUTCTime ctime otime 
    when (dtime <= dtime_bound) $ liftIO (putMVar tref ctime) >> left "too early"
    liftIO (threadDelay 1000000)
    liftIO (print ev)
    homedir <- liftIO $ getHomeDirectory
    r <- liftIO $ readProcess "find" [homedir FP.</> "Dropbox" FP.</> "hoodle","-name","*.hdl","-amin","-1","-print"] "" 
    let nfilelst = lines r 
    liftIO $ print nfilelst 
    liftIO $ mapM_ putStrLn nfilelst 
    pathmap <- getFileIDList
    idmd5lst <- mapM getIDMD5 nfilelst
    let fileidmd5lst =  zip nfilelst idmd5lst 
    let npathmap = foldr (\(f,(i,s)) m -> M.adjust (const (s,f)) i m) pathmap fileidmd5lst 
    -- mapM_ (liftIO . print) (M.toList npathmap)
    let dbfile = homedir FP.</> "Dropbox" FP.</> "hoodleiddb.dat"
        
        rstr = (L.pack . intercalate "\n" . map (\(i,(s,f))->i++" "++s++" "++show f))
                 (M.toList npathmap)
    liftIO $ runResourceT $ sourceLbs rstr $$ sinkFile dbfile 
    -- --     liftIO $ withFile  dbfile WriteMode $ \h -> do 
    liftIO $ print "done"
    -- liftIO $ modifyMVar_ tref (\_ -> return ctime) 
    liftIO $ putMVar tref ctime
    --       writeIORef tref ctime 
    
  case e of 
    Left err -> putStrLn err 
    Right () -> return ()
{-
      -- print pathmap 
      case mpathmap of 
        Nothing -> return () 
        Just pathmap -> do 
          -- system "idfilepathdb" 

-}



main :: IO () 
main = do 
  ctime <- getCurrentTime 
  tref <- newMVar ctime 

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
