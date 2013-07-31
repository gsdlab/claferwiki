module ClaferReader (plugin) where

import Network.Gitit.Interface
import Prelude hiding (id)

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
