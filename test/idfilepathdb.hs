{-# LANGUAGE OverloadedStrings #-}
-- 
-- testing program for attoparsec and sax hoodle parser
-- 
import           Control.Lens
import           Control.Monad
import           Data.Attoparsec 
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Data.Digest.Pure.MD5
import           System.Environment 
import           System.IO
import           System.Posix.Files 
import           System.Process
-- 
import Data.Hoodle.Simple
-- 
import Text.Hoodle.Parse.Attoparsec 
import qualified Text.Hoodle.Migrate.V0_1_999_to_V0_1_9999 as MV
-- import Text.Hoodle.Parse.Conduit 
import Graphics.Hoodle.Render
import Graphics.Hoodle.Render.Type
import Graphics.Rendering.Cairo 

main :: IO () 
main = do 
  r <- readProcess "find" ["/home/wavewave/Dropbox/hoodle","-name","*.hdl","-print"] "" 
  withFile "test.dat" WriteMode $ \h -> do 
    mapM_ (onefile h) (lines r)



-- |  
onefile :: Handle -> FilePath -> IO ()   
onefile fh fp  = do 
  putStrLn $ fp ++ " is now being analyzed " 
  bstr <- B.readFile fp
  eh <- checkVersionAndGetIfHigherVersion bstr
  case eh of 
    Left str -> print str 
    Right h -> do
      fs <- getFileStatus fp 
      let etime = modificationTime fs
          utctime = posixSecondsToUTCTime (realToFrac etime)
      let idstr = B.unpack (view hoodleID h)
          md5str = show (md5 (L.fromChunks [bstr]))
      --     nfilename = "UUID_"++idstr++"_MD5Digest_"++md5str++"_ModTime_"++(show utctime) ++ ".hdl"
      -- B.writeFile nfilename bstr 
      hPutStrLn fh ( idstr ++  " " ++ md5str ++ " " ++ show fp )
      

     
-- | using attoparsec without any built-in xml support 
checkVersionAndGetIfHigherVersion :: B.ByteString -> IO (Either String Hoodle) 
checkVersionAndGetIfHigherVersion bstr = do 
  case parseOnly checkHoodleVersion bstr of 
    Left str -> return (Left str )
    Right v -> do 
      if ( v < "0.1.9999" ) 
        then return (Left "low version") -- MV.migrate bstr
        else return (parseOnly hoodle bstr)

   
