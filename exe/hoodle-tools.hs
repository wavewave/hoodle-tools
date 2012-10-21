-- 
-- testing program for attoparsec and sax hoodle parser
-- 

import           Control.Lens
import           Control.Monad
import           Data.Attoparsec 
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import           Data.Monoid ((<>))
import           System.Environment 
-- 
import Data.Hoodle.Simple
-- 
import Text.Hoodle.Parse.Attoparsec hiding (title) 
-- import Text.Hoodle.Parse.Conduit 
import Text.Hoodle.Builder hiding ((<>))

import Graphics.Hoodle.Render
import Graphics.Hoodle.Render.Type
import Graphics.Rendering.Cairo 

-- |  
main :: IO ()   
main = do 
  args <- getArgs 
  when (length args /= 3) $ error "hoodle-tool file1 file2"  
  attoparsec (args !! 0) $ \h1 -> do 
    attoparsec (args !! 1) $ \h2 -> do 
      let h3 = merge h1 h2
      L.writeFile (args !! 2) (builder h3)
     
-- | 
merge :: Hoodle -> Hoodle -> Hoodle 
merge h1 h2 = 
  let ttl = view title h1 
      pgs1 = view pages h1 
      pgs2 = view pages h2 
  in set title ttl . set pages (pgs1<>pgs2) $ emptyHoodle 
     
-- | using attoparsec without any built-in xml support 
attoparsec :: FilePath -> (Hoodle -> IO ()) -> IO () 
attoparsec fp action = do 
  bstr <- B.readFile fp
  let r = parse hoodle bstr
  case r of 
    Done _ h -> action h 
    _ -> print r 

-- | 
simpleaction :: Hoodle -> IO () 
simpleaction hdl = do 
  let pgs = view pages hdl 
  print (length pgs) 

-- | 
renderjob :: Hoodle -> FilePath -> IO () 
renderjob h ofp = do 
  let p = head (hoodle_pages h) 
  let Dim width height = page_dim p  
  withPDFSurface ofp width height $ \s -> renderWith s $  
    (sequence1_ showPage . map renderPage . hoodle_pages) h 
    
-- | interleaving a monadic action between each pair of subsequent actions
sequence1_ :: (Monad m) => m () -> [m ()] -> m () 
sequence1_ _ []  = return () 
sequence1_ _ [a] = a 
sequence1_ i (a:as) = a >> i >> sequence1_ i as 


