module ClaferWriter (plugin) where

import Network.Gitit.Interface
import Data.Digest.Pure.SHA (sha1, showDigest)
import System.FilePath ((</>))
import System.IO
import Control.Monad.Trans (liftIO)

plugin :: Plugin
plugin = mkPageTransformM readBlock

readBlock :: Block -> PluginM Block
readBlock (CodeBlock (id, classes, namevals) contents)
  | first classes == "clafer" = liftIO $ do
    contents <- getBlock
    return $ CodeBlock (id, classes, namevals) contents
readBlock x = return x

--this is added so that it won't break if the wiki contains code blocks with no headers
first [] = []
first (x:xs) = x

getBlock = do
  contents <- readFile "static/clafer/output.txt"
  let fileLines = lines contents;
      block = unlines (takeWhile (\ x -> not (x == "#break")) fileLines);
  length contents `seq` (writeFile "static/clafer/output.txt" (unlines (tail (dropWhile (\ x -> not (x == "#break")) fileLines))))
  return block
