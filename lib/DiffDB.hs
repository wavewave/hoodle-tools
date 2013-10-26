module DiffDB where

import Data.Maybe (fromJust,mapMaybe)
import qualified Data.Map as M

data DiffType = Same 
              | RightNew (String,FilePath) 
              | LeftNew (String,FilePath) 
              | Conflict (String,FilePath) (String,FilePath)  
              deriving (Show,Eq) 

checkdiff :: M.Map String (String,FilePath) 
          -> M.Map String (String,FilePath) 
          -> M.Map String DiffType
checkdiff olddb newdb = 
  let oks = M.keys olddb 
      nks = M.keys newdb
      ks = oks ++ nks
      rightnew k = (k,(RightNew . fromJust) (M.lookup k newdb))
      compf k = case (M.lookup k olddb,M.lookup k newdb) of  
        (Nothing,Nothing) -> Nothing
        (Just v,Nothing) -> Just (k,LeftNew v)
        (Nothing,Just v) -> Just (k,RightNew v)
        (Just v,Just v') -> 
          if v == v' then Just (k,Same) else Just (k,Conflict v v')
  in (M.fromList . filter ((/=Same).snd) . mapMaybe compf) ks
