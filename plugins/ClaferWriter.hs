module ClaferWriter (plugin) where

import Network.Gitit.Interface
import Control.Monad.Trans (liftIO)

plugin :: Plugin
plugin = mkPageTransformM readBlock

readBlock :: Block -> PluginM Block
readBlock (CodeBlock (id, classes, namevals) contents)
  | "clafer" `elem` classes && (not $ "summary" `elem` classes) = liftIO $ do
    contents <- getBlock
    return $ CodeBlock (id, classes, namevals) contents
readBlock x = return x

--this is added so that it won't break if the wiki contains code blocks with no headers
first [] = []
first (x:xs) = x

getBlock = do
  contents <- readFile "static/clafer/output.txt"
  let fileLines = lines contents;
      block = unlines $ takeWhile (\ x -> x /= "//# FRAGMENT") fileLines
  length contents `seq` (writeFile "static/clafer/output.txt" (unlines $ tail $ dropWhile (\ x -> x /= "//# FRAGMENT") fileLines))
-- the preceding line was taken from StackOverflow:
-- http://stackoverflow.com/questions/2527271/in-haskell-i-want-to-read-a-file-and-then-write-to-it-do-i-need-strictness-ann
  return block
