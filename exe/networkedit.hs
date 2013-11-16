{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as B
import           Data.Monoid
import           Network.Simple.TCP 
import           System.Environment

main :: IO ()
main = do
  [arg0,arg1] <- getArgs 
  connect arg0 arg1 $ \(sock, addr) -> do
    putStrLn $ "Connection established to " ++ show addr
    mbstr <- recv sock 100000
    case mbstr of 
      Nothing -> putStrLn "no get"
      Just bstr -> do 
        send sock (bstr <> "okay")
        return ()

    -- B.putStrLn bstr
    
