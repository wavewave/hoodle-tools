{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as B
import           Data.Monoid
import           Data.UUID.V4
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
    mbstr <- recv sock 100000
    case mbstr of 
      Nothing -> putStrLn "no get"
      Just bstr -> do 
        B.putStrLn bstr 
        tdir <- getTemporaryDirectory
        uuid <- nextRandom
        let fpath = tdir </> show uuid <.> "txt"
        B.writeFile fpath bstr
        system (ed ++ " " ++ fpath)
        nbstr <- B.readFile fpath
        send sock (nbstr)
        return ()

    -- B.putStrLn bstr
    
