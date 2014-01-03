{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Applicative ((<$>))
import           Control.Concurrent (threadDelay)
import           Control.Monad (when,replicateM)
import           Control.Monad.Loops (unfoldM) 
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import qualified Data.Binary as Bi
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.Foldable as F (mapM_,forM_)
import           Data.Maybe (catMaybes)
import           Data.Monoid
import           Data.Time.Clock
-- import           Data.UUID.V4
import           Data.Word
import           Network.Simple.TCP 
import           System.Directory
import           System.FilePath
import           System.Environment
import           System.Process

main :: IO ()
main = do
  [arg0,arg1] <- getArgs
  ed <- getEnv "EDITOR"
  connect arg0 arg1 $ \(sock, addr) -> do
    putStrLn $ "Connection established to " ++ show addr
    mr <- runMaybeT $ do 
      bstr <- MaybeT (recv sock 4)
      let getsize :: B.ByteString -> Word32 
          getsize = Bi.decode . LB.fromChunks . return
          size = (fromIntegral . getsize) bstr 
          
          go s bstr = do 
            bstr1 <- MaybeT (recv sock s)
            let s' = B.length bstr1 
            if s <= s' 
              then return (bstr <> bstr1)
              else go (s-s') (bstr <> bstr1) 
      go size B.empty 

    F.forM_ mr $ \bstr -> do 
      tdir <- getTemporaryDirectory
      ctime <- getCurrentTime
      -- uuid <- nextRandom
      let fpath =  tdir </> show ctime <.> "txt"
      let edws = words ed 
          ed1 = head edws
          eds = tail edws
      
      print (ed1, [eds ++ [fpath] ])
      
      B.writeFile fpath bstr
      (_,_,_,h) <- createProcess (proc ed1 (eds ++ [fpath]))
      waitForProcess h
      --  system (ed ++  " " ++ fpath)
      nbstr <- B.readFile fpath
      let nbstr_size :: Word32 = (fromIntegral . B.length) nbstr
          nbstr_size_binary = (mconcat . LB.toChunks . Bi.encode) nbstr_size
      send sock (nbstr_size_binary <> nbstr)
      return ()
      

