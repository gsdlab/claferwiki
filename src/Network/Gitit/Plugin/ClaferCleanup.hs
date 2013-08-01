module Network.Gitit.Plugin.ClaferCleanup (plugin) where

import Network.Gitit.Interface
-- import Control.Monad.Trans (liftIO)
import Control.Monad (when)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import Language.Clafer.Css

plugin :: Plugin
plugin = mkPageTransformM cleanup

cleanup :: Pandoc -> PluginM Pandoc
cleanup x = do
    request <- askRequest 
    liftIO $ do
        print $ show $ rqPaths request
        createDirectoryIfMissing True "static/clafer/"
        createDirectoryIfMissing True "static/css/"
        tempExist <- doesFileExist "static/clafer/temp.txt"
        when tempExist $ removeFile "static/clafer/temp.txt"
        outputExist <- doesFileExist "static/clafer/output.html"
        when outputExist $ removeFile "static/clafer/output.html"
        nameExist <- doesFileExist "static/clafer/name.txt"
        when nameExist $ removeFile "static/clafer/name.txt"
        writeFile "static/css/clafer.css" css
        return x
