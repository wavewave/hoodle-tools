{-# LANGUAGE OverloadedStrings #-}

import           Control.Lens 
import           Control.Monad
import           Data.Attoparsec
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.ByteString.Base64
import qualified Data.Map as M
import qualified Graphics.UI.Gtk.Poppler.Document as Poppler 
import           Graphics.UI.Gtk (initGUI) 
import           System.Directory
import           System.Environment
import           System.FilePath 
-- from hoodle-platform 
import           Data.Hoodle.Simple as S
import           Text.Hoodle.Parse.Attoparsec 
import           Text.Hoodle.Builder 
-- from this package

splitfunc :: String -> (String,String)
splitfunc str = 
  let (str1,rest1) = break (== ' ') str 
      (_str2,rest2) = break (== ' ') (tail rest1)
      str3 = read (tail rest2)
  in (str1,str3)

-- | 
main :: IO ()   
main = do 
  args <- getArgs 
  when (length args /= 3) $ error "bkgpdfreplace hoodlehome hdlid pdffile"   
  initGUI
  let hoodlehome = args !! 0 
      hdlid = args !! 1 
      pdffile = args !! 2
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
      let fullhdlfile = hoodlehome </> hdlfile 
      let (_,hdlfilename) = splitFileName fullhdlfile
      cdir <- getCurrentDirectory 
      putStrLn $ "back up your file to " ++ (cdir </> hdlfilename)
      copyFile fullhdlfile (cdir </> hdlfilename) 

      mh <- attoparsec fullhdlfile
      case mh of 
        Nothing -> putStrLn "not parsed"
        Just hoo -> do
          nhoo <- embedPDFInHoodle pdffile hoo 
          L.writeFile fullhdlfile . builder $ nhoo 


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
