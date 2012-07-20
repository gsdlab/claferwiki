module ClaferWriter (plugin) where

import Network.Gitit.Interface
import Control.Monad.Trans (liftIO)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>))
import Language.Clafer (addModuleFragment, defaultClaferArgs,
                        CompilerResult(..))
import Language.Clafer.ClaferArgs

plugin :: Plugin
plugin = mkPageTransformM readBlock

readBlock :: Block -> PluginM Block
readBlock (CodeBlock (id, classes, namevals) contents)
  | "clafer" `elem` classes && (not $ "summary" `elem` classes) = liftIO $ do
    contents <- getBlock
    return $ RawBlock "html" ("<div class=\"code\">" ++ contents ++ "</div>")
  {-| "clafer" `elem` classes && "summary" `elem` classes = do
      let args = defaultClaferArgs{mode=Just Html, keep_unused=Just True}
      cfg <- askConfig
      liftIO $ do
      fileName <- readFile "static/clafer/name.txt"
      content <- readFile $ "static/clafer/" ++ fileName ++ ".cfr"
      let CompilerResult {extension = ext,
                          outputCode = output,
                          statistics = stats} = generateGraph args (addModuleFragment args content) "Summary";
      _ <- readProcessWithExitCode "dot" ["-Tsvg", "-o", "static/clafer/summary.svg"] output
      out <- readFile "static/clafer/summary.svg"
      return $ RawBlock "html" (out ++ "<br>\nModule Statistics:<br>\n<span class=\"summary\">" ++ stats ++
                                 "</class><br>\nModule Downloads: <a href=clafer/" ++ fileName ++ ".cfr>[.cfr]</a> <a href=clafer/" ++ fileName ++ ".html>[.html]</a>")-}
readBlock x = return x

--this is added so that it won't break if the wiki contains code blocks with no headers
first [] = []
first (x:xs) = x

getBlock = do
  contents <- readFile "static/clafer/output.html"
  let fileLines = lines contents;
      block = unlines $ takeWhile (\ x -> x /= "<!-- # FRAGMENT -->") fileLines
  length contents `seq` (writeFile "static/clafer/output.html" (unlines $ tail $ dropWhile (\ x -> x /= "<!-- # FRAGMENT -->") fileLines))
-- the preceding line was taken from StackOverflow:
-- http://stackoverflow.com/questions/2527271/in-haskell-i-want-to-read-a-file-and-then-write-to-it-do-i-need-strictness-ann
  return block
