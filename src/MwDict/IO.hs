{-# LANGUAGE Arrows #-}
module MwDict.IO (loadXml, Product (..)) where

import Control.Exception
import Data.Char
import Data.Conduit (($$+-))
import Data.Conduit.Binary (sinkHandle)
import GHC.IO.Exception
import MwDict.Data
import MwDict.Deserialize ()
import Network.HTTP.Base (urlEncode)
import Network.HTTP.Conduit
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.Posix.Env (getEnvDefault)
import Text.XML.HXT.Core

data Product = Learners deriving Show

apiKey :: Product -> IO String
apiKey prod = getEnv $ "MWDICT_API_KEY_" ++ map toUpper (show prod)

xmlUrl :: Product -> String -> String -> String
xmlUrl prod word key =
  "http://www.dictionaryapi.com/api/v1/references/"
  ++
  map toLower (show prod) ++ "/xml/" ++ urlEncode word
  ++
  "?key=" ++ key

fetchXml :: String -> IO FilePath
fetchXml url = withTempFile "mwdict.xml" fetch where
  fetch f fh = do request <- parseUrl url
                  withManager $ \ manager -> do
                    response <- http request manager
                    responseBody response $$+- sinkHandle fh
                    return f

xmlPath :: Product -> String -> IO FilePath
xmlPath prod word = do
  home   <- getEnv "HOME"
  xdg    <- getEnvDefault "XDG_CACHE_HOME"    (home </> ".cache")
  mwdict <- getEnvDefault "MWDICT_CACHE_HOME" (xdg  </> "mwdict")
  let dir = mwdict </> map toLower (show prod)
  createDirectoryIfMissing True dir
  return $ dir </> word <.> "xml"

loadXml :: Bool -> Product -> String -> IOStateArrow s a EntryList
loadXml False prod word = loadXml' prod word $ const removeFile
loadXml True  prod word = proc _ -> do
  file   <- arrIO $ xmlPath prod -< word
  cached <- arrIO doesFileExist  -< file
  if cached
     then loadXml'' file                  -<< ()
     else loadXml' prod word $ cache file -<< ()
  where cache file el tmp | foundEntries el = renameFile' tmp file
                          | otherwise       = removeFile tmp

loadXml' :: Product
         -> String
         -> (EntryList -> FilePath -> IO b)
         -> IOStateArrow s a EntryList
loadXml' prod word action = proc _ -> do
  key  <- arrIO0 $ apiKey prod                -<  ()
  file <- arrIO $ fetchXml . xmlUrl prod word -<  key
  el   <- loadXml'' file                      -<< ()
  arrIO2 action                               -<  (el, file)
  returnA                                     -<  el

loadXml'' :: String -> IOStateArrow s a EntryList
loadXml'' = xunpickleDocument xpickle [ withRemoveWS yes
                                      , withTrace    0
                                      ]

foundEntries :: EntryList -> Bool
foundEntries (EntryList (Left  _)) = False
foundEntries (EntryList (Right _)) = True

withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile pat action = do dir     <- getTemporaryDirectory
                             (f, fh) <- openTempFile dir pat
                             finally (action f fh) $ hClose fh

renameFile' :: FilePath -> FilePath -> IO ()
renameFile' old new =
  renameFile old new `catch`
  \ (IOError _ UnsupportedOperation _ _ _ _) -> do copyFile old new
                                                   removeFile old
