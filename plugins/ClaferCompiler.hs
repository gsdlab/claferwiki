module ClaferCompiler (plugin) where

import Network.Gitit.Interface
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>))
import System.Directory (doesFileExist, removeFile)
import Control.Monad.Trans (liftIO)
import Control.Monad (when)

plugin :: Plugin
plugin = mkPageTransformM callClafer

callClafer :: Block -> PluginM Block
callClafer x = liftIO $ do
  notCompiled <- doesFileExist "static/clafer/temp.txt"
  if notCompiled
     then do compile "static/clafer/temp.txt"
             return x
     else return x

compile file = do
  content <- readFile file
  writeFile "static/clafer/output.txt" ((unlines (addNumbers (lines content) 1)) ++ "\nEnd of file")
  removeFile file

addNumbers :: [String] -> Int -> [String]
addNumbers [] _ = []
addNumbers (x:xs) index
  | x == "#break" = ("End of fragment " ++ (show index)) : "#break" : addNumbers xs (index + 1)
  | otherwise = x : addNumbers xs index
