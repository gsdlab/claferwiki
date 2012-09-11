module ClaferWriter (plugin) where

import Network.Gitit.Interface
import Control.Monad.Trans (liftIO)
import System.Process (readProcessWithExitCode)
import System.Directory (doesFileExist)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>))
import Language.Clafer
import Language.ClaferT
import Language.Clafer.ClaferArgs

plugin :: Plugin
plugin = mkPageTransformM readBlock

readBlock :: Block -> PluginM Block
readBlock block@(CodeBlock (id, classes, namevals) contents)
  | "clafer" `elem` classes && "summary" `elem` classes 
    = summary (Just Graph) True True
  | "clafer" `elem` classes && "graph" `elem` classes
    = summary (Just Graph)  ("stats" `elem` classes) ("links" `elem` classes) 
  | "clafer" `elem` classes && "cvlGraph" `elem` classes
    = summary (Just CVLGraph) ("stats" `elem` classes) ("links" `elem` classes) 
  | "clafer" `elem` classes && not ("graph" `elem` classes) && not ("cvlGraph" `elem` classes) && (("stats" `elem` classes) || ("links" `elem` classes) )
    = summary Nothing ("stats" `elem` classes) ("links" `elem` classes) 
  | "clafer"  `elem` classes && not ("graph" `elem` classes) && not ("cvlGraph" `elem` classes) && not ("stats" `elem` classes) && not ("links" `elem` classes)
    = liftIO $ do
    contents <- getBlock
    return $ RawBlock "html" ("<div class=\"code\">" ++ contents ++ "</div>")
readBlock x = return x

summary graphMode withStats withLinks = do
      let args = defaultClaferArgs{mode=graphMode, keep_unused=Just True}
      liftIO $ do
      fileExists <- doesFileExist "static/clafer/name.txt"
      if fileExists--file may not exist if an error occurred
      then do fileName <- readFile "static/clafer/name.txt"
              content <- readFile $ "static/clafer/" ++ fileName ++ ".cfr"
              graph <- runClaferT args $ do
                  addModuleFragment content
                  parse
                  compile
                  generate
              case graph of
                Right CompilerResult { extension = ext,
                                       outputCode = output,
                                       statistics = stats } -> do
                    _ <- readProcessWithExitCode "dot" ["-Tsvg", "-o", "static/clafer/summary.svg"] output
                    out <- readFile "static/clafer/summary.svg"
                    return $ RawBlock "html" ((if (withGraph graphMode) then out else "") ++ (if (withGraph graphMode) && withStats then "<br>\n" else "") ++ 
                                                (if withStats then "Module Statistics:<br>\n" ++ unlines (map (++"<br>") (lines stats)) else "") ++ (if withLinks && (withStats || (withGraph graphMode)) then "<br>\n" else "") ++
                                                if withLinks then "Module Downloads: <a href=clafer/" ++ fileName ++ ".cfr>[.cfr]</a> <a href=clafer/" ++ fileName ++ ".html>[.html]</a>" else "")
                        where withGraph Nothing = False
                              withGraph _       = True
                Left err -> return $ RawBlock "html" ("<pre>\n" ++ (concatMap handleErr err) ++ "\n</pre>")
                  where handleErr (ClaferErr msg) = "Clafer encountered an error: " ++ msg
                        handleErr (ParseErr ErrPos{modelPos = Pos l c} msg) = "Clafer encountered a parse error at line " ++ show l ++ ", column " ++ show c ++ " " ++ msg
                        handleErr (SemanticErr ErrPos{modelPos = Pos l c} msg) = "Clafer encountered a compilation error at line " ++ show l ++ ", column " ++ show c ++ " " ++ msg						
      else return $ RawBlock "html" "Clafer error: <span class=\"error\">No clafer model found</span>"

--this is added so that it won't break if the wiki contains code blocks with no headers
first [] = []
first (x:xs) = x
rest [] = []
rest (x:xs) = xs

getBlock = do
  contents <- readFile "static/clafer/output.html"
  let fileLines = lines contents;
      block = unlines $ takeWhile (\ x -> x /= "<!-- # FRAGMENT /-->") fileLines
  length contents `seq` (writeFile "static/clafer/output.html" (unlines $ rest $ dropWhile (\ x -> x /= "<!-- # FRAGMENT /-->") fileLines))
-- the preceding line was taken from StackOverflow:
-- http://stackoverflow.com/questions/2527271/in-haskell-i-want-to-read-a-file-and-then-write-to-it-do-i-need-strictness-ann
  return block
