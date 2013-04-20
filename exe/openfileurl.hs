module Main where 

import Network.URI
import Network.HTTP.Base
import System.Environment 
import System.Process

main :: IO () 
main = do  
  -- args <- getArgs 
  -- let url = args !! 0 
  
  str <- readProcess "xclip" ["-o"] "" 
  let fn = urlDecode (drop 7 str)
  readProcess "env" ["LIBOVERLAY_SCROLLBAR=0", "hoodle", fn] "" 
  return ()
  