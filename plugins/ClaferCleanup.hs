module ClaferCleanup (plugin) where

import Network.Gitit.Interface
import Control.Monad.Trans (liftIO)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import Language.Clafer.Css

plugin :: Plugin
plugin = mkPageTransformM cleanup

cleanup :: Block -> PluginM Block
cleanup x = liftIO $ do
  createDirectoryIfMissing True "static/clafer/"
  createDirectoryIfMissing True "static/css/"
  tempExists <- doesFileExist "static/clafer/temp.txt"
  outputExists <- doesFileExist "static/clafer/output.html"
  nameExists <- doesFileExist "static/clafer/name.txt"
  if tempExists
     then do removeFile "static/clafer/temp.txt"
             return x
     else return x
  if outputExists
     then do removeFile "static/clafer/output.html"
             return x
     else return x
  if nameExists
     then do removeFile "static/clafer/name.txt"
             return x
     else return x
  writeFile "static/css/clafer.css" css
  return x
