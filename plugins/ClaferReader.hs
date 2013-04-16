module ClaferReader (plugin) where

import Network.Gitit.Interface
-- import Control.Monad.Trans (liftIO)

plugin :: Plugin
plugin = mkPageTransformM readBlock

readBlock :: Block -> PluginM Block
readBlock (CodeBlock (id, classes, namevals) contents)
  | classes == ["clafer"] = do
  let filepath = "static/clafer/temp.txt"
  liftIO $ do
    _ <- appendFile filepath (contents ++ "\n//# FRAGMENT\n")
    return $ CodeBlock (id, classes, namevals) contents
{-  | "clafer" `elem` classes && "summary" `elem` classes = do
  let filepath = "static/clafer/temp.txt"
  liftIO $ do
    _ <- appendFile filepath "\n//# SUMMARY\n"
    return $ CodeBlock (id, classes, namevals) contents-}
readBlock x = return x

--this is added so that it won't break if the wiki contains code blocks with no headers
first [] = []
first (x:xs) = x
