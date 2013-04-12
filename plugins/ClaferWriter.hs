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
  | "clafer" `elem` classes && "mooviz" `elem` classes 
    = analyzeWithClaferMooViz 
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

withGraph Nothing = False
withGraph _       = True
             
analyzeWithClaferMooViz = do
  liftIO $ do 
  fileName <- readFile "static/clafer/name.txt"
  return $ RawBlock "html" (unlines [
    "<div>" ++
    "<a href=\"http://gsd.uwaterloo.ca:5002/?claferFileURL=http://gsd.uwaterloo.ca:5001/clafer/" ++ 
    fileName ++  
    ".cfr\" target=\"_blank\" " ++
    "style=\"background-color: #ccc;color: white;text-decoration: none;padding: 1px 5px 1px 5px;\" >" ++
    "Analyze with ClaferMooVisualizer" ++
    "</a></div><br>\n"
    ])

summary graphMode withStats withLinks = do
        let argsWithoutRefs = defaultClaferArgs{mode=(if withGraph graphMode then graphMode else Just Graph), keep_unused=Just True, show_references=Just False, noalloyruncommand=Just True}
        let argsWithRefs = defaultClaferArgs{mode=(if withGraph graphMode then graphMode else Just Graph), keep_unused=Just True, show_references=Just True, noalloyruncommand=Just True}
        liftIO $ do
        fileExists <- doesFileExist "static/clafer/name.txt"
        if fileExists--file may not exist if an error occurred
        then do 
                fileName <- readFile "static/clafer/name.txt"
                content <- readFile $ "static/clafer/" ++ fileName ++ ".cfr"
                graphWithoutRefs <- runClaferT argsWithoutRefs $ do
                        addModuleFragment content
                        parse
                        compile
                        generate
                graphWithRefs <- runClaferT argsWithRefs $ do
                        addModuleFragment content
                        parse
                        compile
                        generate     
                case graphWithoutRefs of
                        Right CompilerResult { extension = ext,
                                               outputCode = dotWithoutRefs,
                                               statistics = stats } -> do
                                case graphWithRefs of
                                        Right CompilerResult { outputCode = dotWithRefs } -> do
                                                -- (_, unflattenedDotWithoutRefs, _) <- readProcessWithExitCode "unflatten" [ "-l 1000" ] dotWithoutRefs
                                                -- (_, unflattenedDotWithRefs, _) <- readProcessWithExitCode "unflatten" [ "-l 1000" ] dotWithRefs
                                                (_, outWithoutRefs, _) <- readProcessWithExitCode "dot" [ "-Tsvg" ] dotWithoutRefs
                                                (_, outWithRefs, _) <- readProcessWithExitCode "dot" [ "-Tsvg" ] dotWithRefs
                                                return $ RawBlock "html" ((if (withGraph graphMode) 
                                                                            then (createGraphWithToggle outWithoutRefs outWithRefs) 
                                                                            else "") ++ 
                                                                          (if (withGraph graphMode) && withStats then "<br>\n" else "") ++ 
                                                                          (if withStats 
                                                                            then "<div><b>Module Statistics:</b> \n| " ++ (unlines (map (++ " | " ) (lines stats))) ++ "</div><br>\n" 
                                                                            else "") ++ 
                                                                          (if withLinks && (withStats || (withGraph graphMode)) then "\n" else "") ++
                                                                          (if withLinks then "<div><b>Module Downloads:</b> | <a href=\"/clafer/" ++ fileName ++ ".cfr\">[.cfr]</a> | <a href=\"/clafer/" ++ fileName ++ ".html\">[.html]</a> |</div><br>\n" else ""))
                                        Left err -> return $ RawBlock "html" ("<pre>\n" ++ (concatMap handleErr err) ++ "\n</pre>")
                        Left err -> return $ RawBlock "html" ("<pre>\n" ++ (concatMap handleErr err) ++ "\n</pre>")
        else return $ RawBlock "html" "Clafer error: <span class=\"error\">No clafer model found</span>"

handleErr (ClaferErr msg) = "Clafer encountered an error: " ++ msg
handleErr (ParseErr ErrPos{modelPos = Pos l c} msg) = "Clafer encountered a parse error at line " ++ show l ++ ", column " ++ show c ++ " " ++ msg
handleErr (SemanticErr ErrPos{modelPos = Pos l c} msg) = "Clafer encountered a compilation error at line " ++ show l ++ ", column " ++ show c ++ " " ++ msg                                                


createGraphWithToggle outWithoutRefs outWithRefs = unlines [
    "<div id=\"graphWithoutRefs\" style=\"display:block;width:100%;border:solid lightgray 1px;overflow-x:auto;\" ondblclick=\"" ++ showRefs  ++ "\">",
    outWithoutRefs, 
    "</div>",
    "<div id=\"graphWithRefs\" style=\"display:none;width:100%;border:solid lightgray 1px;overflow-x:auto;\" ondblclick=\"" ++ hideRefs  ++ "\">",
    outWithRefs, 
    "</div>" ]

showRefs =
  "var gwr=document.getElementById('graphWithRefs'); gwr.style.display='block'; gwr.scrollLeft=this.scrollLeft; this.style.display='none';"
  
hideRefs = 
  "var gwor=document.getElementById('graphWithoutRefs'); gwor.style.display='block'; gwor.scrollLeft=this.scrollLeft;this.style.display='none';"
    
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
