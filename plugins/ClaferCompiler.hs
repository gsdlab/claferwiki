module ClaferCompiler (plugin) where

import Network.Gitit.Interface
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>))
-- from the utf8-string package on HackageDB:
import Data.ByteString.Lazy.UTF8 (fromString)
-- from the SHA package on HackageDB:
import Data.Digest.Pure.SHA (sha1, showDigest)
import System.Directory (doesFileExist, removeFile)
import Control.Monad.Trans (liftIO)
import Control.Monad (when)
import Language.Clafer (generateM, compileM, addModuleFragment, defaultClaferArgs, CompilerResult(..))
import Language.Clafer.ClaferArgs
import Data.List (intercalate)
plugin :: Plugin
plugin = mkPageTransformM callClafer

callClafer :: Block -> PluginM Block
callClafer (CodeBlock (id, classes, namevals) contents)
  | first classes == "clafer" = liftIO $ do
  notCompiled <- doesFileExist "static/clafer/temp.txt"
  if notCompiled
     then do compile "static/clafer/temp.txt" defaultClaferArgs{mode=Just Html, keep_unused=Just True}
             return (CodeBlock (id, classes, namevals) contents)
     else return (CodeBlock (id, classes, namevals) contents)
callClafer x = return x

compile file args = do
  content <- readFile file
  let CompilerResult {extension = ext,
                      outputCode = output,
                      statistics = stats} = generateM args (compileM args (addModuleFragment args content));
  writeFile ("static/clafer/" ++ uniqueName content ++ "." ++ ext)
            ("<pre>" ++ stats ++ "</pre><br>\n" ++ output)
  writeFile "static/clafer/output.txt" (unlines $ addNumbers (lines content) 1)
  removeFile file

--this is added so that it won't break if the wiki contains code blocks with no headers
first [] = []
first (x:xs) = x

-- | Generate a unique filename given the file's contents.
uniqueName :: String -> String
uniqueName = showDigest . sha1 . fromString

addNumbers :: [String] -> Int -> [String]
addNumbers [] _ = []
addNumbers (x:xs) index
  | x == "//# FRAGMENT" = ("End of fragment " ++ (show index)) : "//# FRAGMENT" : addNumbers xs (index + 1)
  | otherwise = x : addNumbers xs index
