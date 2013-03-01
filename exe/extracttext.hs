{-# LANGUAGE StandaloneDeriving, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

import           Control.Applicative 
import           Control.Monad 
import qualified Data.Foldable as F
import qualified Data.Traversable as T 
import           Data.List 
import           Data.Maybe 
import           System.Directory 
import           System.Directory.Tree
import           System.Environment
import           System.FilePath 
import           System.Process

import           Data.Attoparsec hiding (take)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Hoodle.Simple
import Text.Hoodle.Parse.Attoparsec hiding (svg_text)

data LTree a = B a [LTree a] 

deriving instance (Show a) => Show (LTree a)

deriving instance Functor LTree
deriving instance F.Foldable LTree
-- deriving instance T.Traversable LTree

-- | using attoparsec without any built-in xml support 
attoparsec :: FilePath -> IO (Maybe Hoodle) 
attoparsec fp = do 
  bstr <- B.readFile fp
  let r = parse hoodle bstr
  case r of 
    Done _ h -> return (Just h)
    _ -> return Nothing  

-- | extract text 
extractTextInHoodle :: Hoodle -> [String] 
extractTextInHoodle = extractTextInLayer <=< page_layers <=< hoodle_pages

extractTextInLayer :: Layer -> [String] 
extractTextInLayer lyr = do 
    i <- layer_items lyr 
    case i of
      ItemStroke _ -> []
      ItemImage _ -> [] 
      ItemSVG svg -> maybe [] (return. C.unpack) (svg_text svg)  


isHdl = ( == ".hdl") <$> takeExtension 

isFile (File _ _) = True
isFile _ = False

takeFile x | isFile x = (Just . file) x 
takeFile x | otherwise = Nothing 

isDirectory (Dir _ _) = True 
isDirectory _ = False 

takeDir x | isDirectory x = (Just . name) x 
takeDir x | otherwise = Nothing 

emptyB = B "" []

startB dir = B dir []

buildLTree ctxt (Failed _ _) = Nothing 
buildLTree ctxt (File _ _) = Nothing 
buildLTree (B ctxt _) (Dir n ds) = 
  let nb = B (ctxt </> n) [] 
  in Just (B (ctxt </> n) (mapMaybe (buildLTree nb) ds))

  
 
splitprefix :: FilePath -> FilePath -> Either FilePath (FilePath,FilePath)
splitprefix fp ofp | isPrefixOf fp ofp = Right (fp, (drop (length fp) ofp))
splitprefix fp ofp | otherwise = Left ofp 

replaceprefix :: (FilePath,FilePath) -> FilePath -> FilePath 
replaceprefix (op,np) = either id (\(_,x)->np++x) . splitprefix op 

splitsuffix :: FilePath -> FilePath -> Either FilePath (FilePath,FilePath)
splitsuffix fp ofp | isSuffixOf fp ofp = Right (take (length ofp - length fp) ofp, fp)
splitsuffix fp ofp | otherwise = Left ofp 

replacesuffix :: (FilePath,FilePath) -> FilePath -> FilePath 
replacesuffix (op,np) = either id (\(x,_)->x++np) . splitsuffix op 


createDirIfNotExist fp = 
    liftM not (doesDirectoryExist fp)
    >>= \b -> when b (createDirectory fp)
    
main = do 
  cwd <- getCurrentDirectory
  (r :/ r') <- build cwd
  let files = catMaybes . map takeFile . flattenDir $ r'
      hdlfiles = filter isHdl $ files  
  let act x = do 
        hdl <- attoparsec x
        let texts = maybe [] extractTextInHoodle hdl 
            r = map (\y->(x,y)) texts
        when ((not.null) r) $ print r  

  mapM_ act hdlfiles 
