{-
 Copyright (C) 2012-2014 Michal Antkiewicz, Chris Walker <http://gsd.uwaterloo.ca>

 Permission is hereby granted, free of charge, to any person obtaining a copy of
 this software and associated documentation files (the "Software"), to deal in
 the Software without restriction, including without limitation the rights to
 use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 of the Software, and to permit persons to whom the Software is furnished to do
 so, subject to the following conditions:

 The above copyright notice and this permission notice shall be included in all
 copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 SOFTWARE.
-}
{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}
-- | A plugin for Gitit which integrates the Clafer compiler and links to other
-- Clafer web tools: <http://https://github.com/gsdlab/ClaferIDE ClaferIDE>,
-- <http://https://github.com/gsdlab/ClaferConfigurator ClaferConfigurator>, and
-- <http://https://github.com/gsdlab/ClaferMooVisualizer ClaferMooVisualizer>
module Network.Gitit.Plugin.ClaferWiki (plugin) where

import Network.Gitit.Interface

import Control.Monad
import Control.Monad.Trans.State
import Data.List
import Data.List.Split
import qualified Data.Map as Map
import Data.String.Utils (replace)
import qualified Data.Text as T
import Network.BSD (getHostName)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Process (readProcessWithExitCode)

import Language.Clafer
import Language.Clafer.Css as Css
import Language.Clafer.Generator.Html (highlightErrors)

-- | claferWiki collects Clafer code from .clafer code blocks, renders as HTML and graph,
--   and replaces the original blocks with RawBlocks containing the results
plugin :: Plugin
plugin = mkPageTransformM claferWiki

claferWiki :: Pandoc -> PluginM Pandoc
claferWiki pandoc = do
    -- make sure the directories and clafer.css exist
    liftIO $ do
        createDirectoryIfMissing True "static/clafer/"
        createDirectoryIfMissing True "static/css/"
        cssExist <- doesFileExist "static/css/clafer.css"
        unless cssExist $ writeFile "static/css/clafer.css" css

    serverURL <- liftIO getHostName
    pageName <- getPageName
    config <- askConfig

    let
        serverPort = show $ portNumber config
        -- produce the required outputs in a single compilation
        allCompilationResults = compileFragments fragments claferModes

        htmlCode = extractOutput allCompilationResults Html
        htmlCodeFragments = splitOn "\n<!-- # FRAGMENT /-->\n" htmlCode

        stats = maybe "No model." statistics $ extractCompilerResult allCompilationResults Html

        dotGraph = maybe "" outputCode $ extractCompilerResult allCompilationResults Graph

        dotCVLGraph = maybe "" outputCode $ extractCompilerResult allCompilationResults CVLGraph

    -- render the graphs to SVG using dot
    (_, svgGraphWithoutRefs, _) <- liftIO $ readProcessWithExitCode "dot" [ "-Tsvg" ] dotGraph
    (_, svgGraphWithRefs, _) <- liftIO $ readProcessWithExitCode "dot" [ "-Tsvg" ] $ changeTransparentToLightGray dotGraph
    (_, svgCVLGraph, _) <- liftIO $ readProcessWithExitCode "dot" [ "-Tsvg" ] dotCVLGraph

        -- using the WikiEnv as state, replace clafer code blocks with appropriate results:
        -- html rendering of clafer code, graph rendering, download links, ide, configurator, and visualizer buttons
    let
        initialWikiEnv = WikiEnv {
                            we_fileName = pageName,
                            we_serverURL = serverURL,
                            we_serverPort = serverPort,
                            we_htmlCodeFragments = htmlCodeFragments,
                            we_stats = stats,
                            we_graphNo = 0,     -- needed to construct unique IDs of <div> for graphs
                            we_svgGraphWithRefs = svgGraphWithRefs,
                            we_svgGraphWithoutRefs = svgGraphWithoutRefs,
                            we_svgCVLGraph = svgCVLGraph
                         }
        newPandoc = evalState (bottomUpM replaceClaferWikiBlocks pandoc) initialWikiEnv

    -- save original model
    liftIO $ writeFile ("static/clafer/" ++ pageName ++ ".cfr") completeModel
    -- save html version
    liftIO $ writeFile ("static/clafer/" ++ pageName ++ ".html") $ selfContained htmlCode

    return newPandoc
    where
        -- collect clafer model fragments
        fragments :: [String]
        fragments = queryWith addFragment pandoc

        -- collects compiler modes depending on the kinds of blocks on the page
        claferModes :: [ ClaferMode ]
        claferModes = nub $ queryWith addMode pandoc

        fragmentedModel :: String
        fragmentedModel = intercalate "//# FRAGMENT\n" fragments

        completeModel :: String
        completeModel = intercalate "\n" fragments

        addFragment :: Block -> [String]
        addFragment (CodeBlock (_, [ "clafer" ], _) code) = [ T.unpack code ++ "\n" ]
        addFragment _                                     = []

        addMode :: Block -> [ClaferMode]
        addMode (CodeBlock (_, [ "clafer" ], _) _)              = [Html]
        addMode (CodeBlock (_, [ "clafer", "graph" ], _) _)     = [Graph]
        addMode (CodeBlock (_, [ "clafer", "summary" ], _) _)   = [Graph]
        addMode (CodeBlock (_, [ "clafer", "cvlGraph" ], _) _)  = [CVLGraph]
        addMode (CodeBlock (_, [ "clafer", "cvlgraph" ], _) _)  = [CVLGraph]
        addMode _                                               = []

        extractCompilerResult :: Either [ClaferErr] (Map.Map ClaferMode CompilerResult) -> ClaferMode -> Maybe CompilerResult
        extractCompilerResult result claferMode = either (const Nothing) (Map.lookup claferMode) result

        extractOutput :: Either [ClaferErr] (Map.Map ClaferMode CompilerResult) -> ClaferMode -> String
        extractOutput (Right compilerResultMap) claferMode =
            case Map.lookup claferMode compilerResultMap of
                Just CompilerResult{ outputCode } -> outputCode
                Just NoCompilerResult{ reason } -> "Error: No " ++ show claferMode ++ " output. Reason:" ++ reason
                Nothing -> "Error: No " ++ show claferMode ++ " output."
        extractOutput (Left err) _ = highlightErrors fragmentedModel err

        selfContained htmlCode =
            concat [
                Css.header,
                "<style>",
                Css.css,
                "</style>",
                "</head>\n<body>\n",
                htmlCode,
                "</body>\n</html>"
            ]

-- | Environment (state) for rewriting the page
data WikiEnv = WikiEnv {
                    we_fileName :: String,
                    we_serverURL :: String,
                    we_serverPort :: String,
                    -- | code fragments are consumed
                    we_htmlCodeFragments :: [ String ],
                    we_stats :: String,
                    we_graphNo :: Int,
                    we_svgGraphWithRefs :: String,
                    we_svgGraphWithoutRefs :: String,
                    we_svgCVLGraph :: String
               }


replaceClaferWikiBlocks :: Block -> State WikiEnv Block
replaceClaferWikiBlocks (CodeBlock (_, [ "clafer" ], _) _) = do
    wikiEnv <- get
    case we_htmlCodeFragments wikiEnv of
        (fragment:fragments) -> do
            put $ wikiEnv { we_htmlCodeFragments = fragments }
            return $ RawBlock "html" ("<div class=\"code\">" <> T.pack fragment <> "</div>")
        []                   -> return $ Plain []

replaceClaferWikiBlocks (CodeBlock (_, [ "clafer", "links" ], _) _) = do
    fileName <- gets we_fileName
    return $ RawBlock "html" $ renderLinks (T.pack fileName)

replaceClaferWikiBlocks (CodeBlock (_, [ "clafer", "stats" ], _) _) = do
    stats <- gets we_stats
    return $ RawBlock "html" $ renderStats (T.pack stats)

replaceClaferWikiBlocks (CodeBlock (_, [ "clafer", "graph" ], _) _) = do
    wikiEnv <- get
    graphNo <- gets we_graphNo
    svgGraphWithRefs <- gets we_svgGraphWithRefs
    svgGraphWithoutRefs <- gets we_svgGraphWithoutRefs
    put $ wikiEnv { we_graphNo = graphNo + 1 }
    return $ RawBlock "html" $ renderGraphWithToggle (T.pack svgGraphWithoutRefs) (T.pack svgGraphWithRefs) graphNo

replaceClaferWikiBlocks (CodeBlock (_, [ "clafer", "cvlGraph" ], _) _) =  do
    svgCVLGraph <- gets we_svgCVLGraph
    return $ RawBlock "html" $ renderGraph (T.pack svgCVLGraph)

replaceClaferWikiBlocks (CodeBlock (_, [ "clafer", "cvlgraph" ], _) _) =  do
    svgCVLGraph <- gets we_svgCVLGraph
    return $ RawBlock "html" $ renderGraph (T.pack svgCVLGraph)

replaceClaferWikiBlocks (CodeBlock (_, [ "clafer", "summary" ], _) _) =  do
    wikiEnv <- get
    fileName <- gets we_fileName
    stats <- gets we_stats
    graphNo <- gets we_graphNo
    svgGraphWithRefs <- gets we_svgGraphWithRefs
    svgGraphWithoutRefs <- gets we_svgGraphWithoutRefs
    put $ wikiEnv { we_graphNo = graphNo + 1 }
    return $ RawBlock "html" $ renderSummary fileName stats svgGraphWithoutRefs svgGraphWithRefs graphNo

replaceClaferWikiBlocks (CodeBlock (_, [ "clafer", "mooviz" ], _) _) =  do
    wikiEnv <- get
    return $ renderAnalyzeWithClaferMooViz (we_fileName wikiEnv) (we_serverURL wikiEnv) (we_serverPort wikiEnv)

replaceClaferWikiBlocks (CodeBlock (_, [ "clafer", "config" ], _) _) =  do
    wikiEnv <- get
    return $ renderConfigureWithClaferConfigurator (we_fileName wikiEnv) (we_serverURL wikiEnv) (we_serverPort wikiEnv)

replaceClaferWikiBlocks (CodeBlock (_, [ "clafer", "ide" ], _) _) =  do
    wikiEnv <- get
    return $ renderAddOpenInIDE (we_fileName wikiEnv) (we_serverURL wikiEnv) (we_serverPort wikiEnv)

replaceClaferWikiBlocks block = return block

renderLinks :: T.Text -> T.Text
renderLinks fileName =
    "<div><b>Module Downloads:</b> | <a href=\"/clafer/" <>
    fileName <>
    ".cfr\">[.cfr]</a> | <a href=\"/clafer/" <>
    fileName <>
    ".html\">[.html]</a> |</div><br>\n"

renderStats :: T.Text -> T.Text
renderStats stats =
    "<div><b>Module Statistics:</b> \n| " <>
    T.intercalate " | " (T.lines stats) <>
    " |</div><br>\n"


renderGraphWithToggle :: T.Text           -> T.Text        -> Int      -> T.Text
renderGraphWithToggle    svgGraphWithoutRefs svgGraphWithRefs graphNo   = T.unlines [
    "<div id=\"" <> renderGraphId False graphNo <>  "\" style=\"display:block;width:100%;border:solid lightgray 1px;overflow-x:auto;\" ondblclick=\"" <> renderShowRefs graphNo <> "\">",
    svgGraphWithoutRefs,
    "</div>",
    "<div id=\"" <> renderGraphId True graphNo <>  "\" style=\"display:none;width:100%;border:solid lightgray 1px;overflow-x:auto;\" ondblclick=\"" <> renderHideRefs graphNo <> "\">",
    svgGraphWithRefs,
    "</div>" ]

renderGraph :: T.Text  -> T.Text
renderGraph    svgGraph = T.unlines [
    "<div style=\"display:block;width:100%;border:solid lightgray 1px;overflow-x:auto;\">",
    svgGraph,
    "</div>" ]

renderShowRefs :: Int    -> T.Text
renderShowRefs    graphNo =
  "var gwr=document.getElementById('" <> renderGraphId True graphNo <>  "'); gwr.style.display='block'; gwr.scrollLeft=this.scrollLeft; this.style.display='none';"

renderHideRefs :: Int    -> T.Text
renderHideRefs    graphNo =
  "var gwor=document.getElementById('" <> renderGraphId False graphNo <>  "'); gwor.style.display='block'; gwor.scrollLeft=this.scrollLeft;this.style.display='none';"

renderGraphId :: Bool -> Int    -> T.Text
renderGraphId    True    graphNo = T.pack $ "graphWithRefs" ++ show graphNo
renderGraphId    False   graphNo = T.pack $ "graphWithoutRefs" ++ show graphNo

renderSummary :: String -> String -> String           -> String        -> Int    -> T.Text
renderSummary    fileName  stats     svgGraphWithoutRefs svgGraphWithRefs graphNo =
    renderGraphWithToggle (T.pack svgGraphWithoutRefs) (T.pack svgGraphWithRefs) graphNo <>
    renderStats (T.pack stats) <>
    renderLinks (T.pack fileName)


compileFragments :: [ String ] -> [ ClaferMode ] -> Either [ClaferErr] (Map.Map ClaferMode CompilerResult)
compileFragments    fragments     claferModes    =
    -- compile all clafer code
    runClafer defaultClaferArgs{
                mode=claferModes,
                keep_unused=True,
                add_comments=True,
                show_references=False } $ do
                                            mapM_ addModuleFragment fragments
                                            parse
                                            iModule <- desugar Nothing
                                            compile iModule
                                            generate


renderAnalyzeWithClaferMooViz :: String -> String -> String -> Block
renderAnalyzeWithClaferMooViz fileName serverURL serverPort =
    RawBlock "html" $ T.pack (unlines [
      "<div>" ++
      "<a href=\"http://" ++ serverURL ++ ":8092/?claferFileURL=http://" ++ serverURL ++ ":" ++ serverPort ++ "/clafer/" ++
      fileName ++
      ".cfr\" target=\"_blank\" " ++
      "style=\"background-color: #ccc;color: white;text-decoration: none;padding: 1px 5px 1px 5px;\" >" ++
      "Analyze with ClaferMooVisualizer" ++
      "</a></div><br>\n"
      ])

renderConfigureWithClaferConfigurator :: String -> String -> String -> Block
renderConfigureWithClaferConfigurator fileName serverURL serverPort =
    RawBlock "html" $ T.pack (unlines [
      "<div>" ++
      "<a href=\"http://" ++ serverURL ++ ":8093/?claferFileURL=http://" ++ serverURL ++ ":" ++ serverPort ++ "/clafer/" ++
      fileName ++
      ".cfr\" target=\"_blank\" " ++
      "style=\"background-color: #ccc;color: white;text-decoration: none;padding: 1px 5px 1px 5px;\" >" ++
      "Configure with ClaferConfigurator" ++
      "</a></div><br>\n"
      ])

renderAddOpenInIDE :: String -> String -> String -> Block
renderAddOpenInIDE fileName serverURL serverPort =
    RawBlock "html" $ T.pack (unlines [
      "<div>" ++
      "<a href=\"http://" ++ serverURL ++ ":8094/?claferFileURL=http://" ++ serverURL ++ ":" ++ serverPort ++ "/clafer/" ++
      fileName ++
      ".cfr\" target=\"_blank\" " ++
      "style=\"background-color: #ccc;color: white;text-decoration: none;padding: 1px 5px 1px 5px;\" >" ++
      "Open in ClaferIDE" ++
      "</a></div><br>\n"
      ])

getPageName:: PluginM String
getPageName = replace " " "_" . replace "/" "_" . pgPageName . ctxLayout <$> getContext

changeTransparentToLightGray :: String -> String
changeTransparentToLightGray = replace "color=transparent" "color=lightgray"
