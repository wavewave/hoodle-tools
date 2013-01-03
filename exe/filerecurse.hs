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

data LTree a = B a [LTree a] 

deriving instance (Show a) => Show (LTree a)

deriving instance Functor LTree
deriving instance F.Foldable LTree
-- deriving instance T.Traversable LTree

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
    
makePDF (x,y) = system $ "/home/wavewave/repo/src/hoodle-parser/examples/parsetest atto " ++ x ++ " " ++ y 

main = do 
  -- args <- getArgs 
  -- let newbase = args !! 0
  cwd <- getCurrentDirectory
  (r :/ r') <- build cwd
  let files = catMaybes . map takeFile . flattenDir $ r'
      hdlfiles = filter isHdl $ files  
      nhdlfiles = map (replaceprefix ("/home/wavewave/Dropbox","/home/wavewave/test")) hdlfiles 
      npdffiles = map (replacesuffix ("hdl","pdf")) nhdlfiles 
  
  
  let dirs = catMaybes . map takeDir . flattenDir $ r'
      
 
  let ltree = buildLTree (startB r) r'
      ltreelst = F.foldr (:) [] (fromJust ltree) --  F.toList ltree
      ntreelst = map (replaceprefix ("/home/wavewave/Dropbox","/home/wavewave/test")) ltreelst
      
  mapM_ print dirs 
  mapM_ print ntreelst
  mapM_ createDirIfNotExist ntreelst

  let cplist = zipWith (,) hdlfiles npdffiles
  mapM_  print cplist 
  -- mapM_ (uncurry copyFile) cplist 
  mapM_ makePDF cplist 

-- mapM_ print ltreelst 
  -- print fromJust ltree
  -- putStrLn $ "length = " ++ show (length ltreelst)
