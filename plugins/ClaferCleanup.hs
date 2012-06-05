module ClaferCleanup (plugin) where

import Network.Gitit.Interface
import Control.Monad.Trans (liftIO)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)

plugin :: Plugin
plugin = mkPageTransformM cleanup

cleanup :: Block -> PluginM Block
cleanup x = liftIO $ do
  createDirectoryIfMissing True "static/clafer/"
  tempExists <- doesFileExist "static/clafer/temp.txt"
  outputExists <- doesFileExist "static/clafer/output.txt"
  if tempExists
     then do removeFile "static/clafer/temp.txt"
             return x
     else return x
  if outputExists
     then do removeFile "static/clafer/output.txt"
             return x
     else return x
