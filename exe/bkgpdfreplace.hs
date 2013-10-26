{-# LANGUAGE OverloadedStrings #-}

import Control.Lens 
import Control.Monad
import Control.Monad.State 
import Data.Attoparsec
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as L
import Data.ByteString.Base64
import qualified Data.Map as M
import Graphics.Rendering.Cairo
import qualified Graphics.UI.Gtk.Poppler.Document as Poppler 
-- import Graphics.UI.Gtk.Poppler.Page as PopplerPage
import Graphics.UI.Gtk hiding (get,set)
import System.Directory
import System.Environment
import System.FilePath 
-- from hoodle-platform 
import Data.Hoodle.Simple as S
import Text.Hoodle.Parse.Attoparsec 
import Text.Hoodle.Builder 
-- from this package
-- import Graphics.Hoodle.Render.SimpleNew 
-- import Graphics.Hoodle.Render.Simple 

splitfunc :: String -> (String,String)
splitfunc str = 
  let (str1,rest1) = break (== ' ') str 
      (str2,rest2) = break (== ' ') (tail rest1)
      str3 = read (tail rest2)
  in (str1,str3)


-- | 
main :: IO ()   
main = do 
  args <- getArgs 
  when (length args /= 2) $ error "bkgpdfreplace hdlid pdffile"   
  initGUI
  let hdlid = args !! 0 
      pdffile = args !! 1
  homedir <- getHomeDirectory
  let hdldb = homedir </> "Dropbox" </> "hoodleiddb.dat"

  b <- doesFileExist hdldb 
  when (not b) $ error ("no " ++ hdldb)

  dbstr <- readFile hdldb
  let ls = lines dbstr 
      alist = map splitfunc ls
      pathmap = M.fromList alist 
      mpath = M.lookup hdlid pathmap 
  case mpath of 
    Nothing -> error (" no such id = " ++ hdlid )
    Just hdlfile -> do 
      let (_,hdlfilename) = splitFileName hdlfile
      cdir <- getCurrentDirectory 
      putStrLn $ "back up your file to " ++ (cdir </> hdlfilename)
      copyFile hdlfile (cdir </> hdlfilename) 

      mh <- attoparsec hdlfile
      case mh of 
        Nothing -> print "not parsed"
        Just hoo -> do
          nhoo <- embedPDFInHoodle pdffile hoo 
          L.writeFile hdlfile . builder $ nhoo 


createPage :: Dimension -> Int -> Page
createPage dim n = let bkg = BackgroundEmbedPdf "embedpdf" n 
                   in Page dim bkg [emptyLayer]


-- | 
embedPDFInHoodle :: FilePath -> Hoodle -> IO Hoodle
embedPDFInHoodle pdffile hdl = do 
    bstr <- L.readFile pdffile 
    let b64str = (encode . B.concat . L.toChunks) bstr 
        ebdsrc = Just ("data:application/x-pdf;base64," <> b64str)

        pgs = view pages hdl 
        pg1 = head pgs 
        dim = view dimension pg1
        -----------
        npg = foldl f 0 pgs
                where f n pg = let bkg = view S.background pg
                               in case bkg of 
                                    BackgroundEmbedPdf _ m -> if n < m then m else n 
                                    _ -> n 
        -- npg = length pgs 
        nhdl1 = set embeddedPdf ebdsrc hdl
    putStrLn $ " npg = " ++ show npg
    cpath_pdffile <- canonicalizePath pdffile 
    mdoc <- Poppler.documentNewFromFile ("file://localhost" ++ cpath_pdffile) Nothing
    case mdoc of 
      Nothing -> return hdl 
      Just doc -> do 
        n <- Poppler.documentGetNPages doc 
        let npgs = map (createPage dim) [npg+1,npg+2..n] 
        print n 
        return (set pages (pgs++npgs) nhdl1) 

-- | 
attoparsec :: FilePath -> IO (Maybe Hoodle)
attoparsec fp = do 
  bstr <- B.readFile fp
  let r = parse hoodle bstr
  case r of 
    Done _ h -> return (Just h)
    _ -> return Nothing 
