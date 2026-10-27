{-
 Copyright 2012-2019 Generative Software Development Lab <http://gsd.uwaterloo.ca>
 Copyright 2019-2023 Intelligent Systems Development Lab <https://uwaterloo.ca/wise-lab/>

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

import Control.Applicative ((<|>))
import Control.Monad (unless)
import Control.Monad.Trans.State (evalState, get, gets, put, State)
import Data.ByteString.UTF8 (toString)
import Data.Char (isDigit, isSpace)
import Data.List (dropWhileEnd, intercalate, isPrefixOf, isSuffixOf, nub, tails)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.String.Utils (replace)
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (Builder,  fromString, toLazyText, singleton)
import Happstack.Server (Request, getHeader)
import Network.BSD (getHostName)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (lookupEnv)
import System.Process (readProcessWithExitCode)

import Language.Clafer
import Language.Clafer.Css as Css (css, header)
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

    hostName <- liftIO getHostName
    request <- askRequest
    pageName <- getPageName
    config <- askConfig
    [mooVizSetting, configuratorSetting, ideSetting] <-
        liftIO $ mapM lookupEnv ["CLAFERMOOVISUALIZER_URL", "CLAFERCONFIGURATOR_URL", "CLAFERIDE_URL"]

    let
        address = wikiAddress hostName (show $ portNumber config) request
        -- produce the required outputs in a single compilation
        allCompilationResults = compileFragments fragments claferModes

        htmlCode = extractOutput allCompilationResults Html
        htmlCodeFragments :: [String]
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
                            we_wikiURL = wa_wikiURL address,
                            we_mooVizURL = toolURL address "8092" mooVizSetting,
                            we_configuratorURL = toolURL address "8093" configuratorSetting,
                            we_ideURL = toolURL address "8094" ideSetting,
                            we_htmlCodeFragments = htmlCodeFragments,
                            we_stats = stats,
                            we_graphNo = 0,     -- needed to construct unique IDs of <div> for graphs
                            we_svgGraphWithRefs = inlineSvg svgGraphWithRefs,
                            we_svgGraphWithoutRefs = inlineSvg svgGraphWithoutRefs,
                            we_svgCVLGraph = inlineSvg svgCVLGraph
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
        fragments = queryWith extractFragment pandoc

        -- collects compiler modes depending on the kinds of blocks on the page
        claferModes :: [ ClaferMode ]
        claferModes = nub $ queryWith addMode pandoc

        fragmentedModel :: String
        fragmentedModel = intercalate "//# FRAGMENT\n" fragments

        completeModel :: String
        completeModel = intercalate "\n" fragments

        extractFragment :: Block -> [String]
        extractFragment (CodeBlock (_, [ "clafer" ], _) code) = [ T.unpack code ++ "\n" ]
        extractFragment _                                     = []

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

        selfContained :: String -> String
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
                    -- | the wiki's URL as the browser addressed it, ending with /
                    we_wikiURL :: String,
                    -- | the web tools' URLs, ending with /
                    we_mooVizURL :: String,
                    we_configuratorURL :: String,
                    we_ideURL :: String,
                    -- | code fragments are consumed
                    we_htmlCodeFragments :: [ String ],
                    we_stats :: String,
                    we_graphNo :: Int,
                    we_svgGraphWithRefs :: String,
                    we_svgGraphWithoutRefs :: String,
                    we_svgCVLGraph :: String
               }


mkHtmlBlock :: Builder -> Block
mkHtmlBlock = RawBlock "html" . toStrict . toLazyText

replaceClaferWikiBlocks :: Block -> State WikiEnv Block
replaceClaferWikiBlocks (CodeBlock (_, [ "clafer" ], _) _) = do
    wikiEnv <- get
    case we_htmlCodeFragments wikiEnv of
        (fragment:fragments) -> do
            put $ wikiEnv { we_htmlCodeFragments = fragments }
            return $ mkHtmlBlock $ "<div class=\"code\">" <> fromString fragment <> "</div>"
        []                   -> return $ Plain []

replaceClaferWikiBlocks (CodeBlock (_, [ "clafer", "links" ], _) _) = do
    fileName <- gets we_fileName
    return $ mkHtmlBlock $ renderLinks fileName

replaceClaferWikiBlocks (CodeBlock (_, [ "clafer", "stats" ], _) _) = do
    stats <- gets we_stats
    return $ mkHtmlBlock $ renderStats stats

replaceClaferWikiBlocks (CodeBlock (_, [ "clafer", "graph" ], _) _) = do
    wikiEnv <- get
    graphNo <- gets we_graphNo
    svgGraphWithRefs <- gets we_svgGraphWithRefs
    svgGraphWithoutRefs <- gets we_svgGraphWithoutRefs
    put $ wikiEnv { we_graphNo = graphNo + 1 }
    return $ mkHtmlBlock $ renderGraphWithToggle svgGraphWithoutRefs svgGraphWithRefs graphNo

replaceClaferWikiBlocks (CodeBlock (_, [ "clafer", "cvlGraph" ], _) _) =  do
    svgCVLGraph <- gets we_svgCVLGraph
    return $ mkHtmlBlock $ renderGraph svgCVLGraph

replaceClaferWikiBlocks (CodeBlock (_, [ "clafer", "cvlgraph" ], _) _) =  do
    svgCVLGraph <- gets we_svgCVLGraph
    return $ mkHtmlBlock $ renderGraph svgCVLGraph

replaceClaferWikiBlocks (CodeBlock (_, [ "clafer", "summary" ], _) _) =  do
    wikiEnv <- get
    fileName <- gets we_fileName
    stats <- gets we_stats
    graphNo <- gets we_graphNo
    svgGraphWithRefs <- gets we_svgGraphWithRefs
    svgGraphWithoutRefs <- gets we_svgGraphWithoutRefs
    put $ wikiEnv { we_graphNo = graphNo + 1 }
    return $ mkHtmlBlock $ renderSummary fileName stats svgGraphWithoutRefs svgGraphWithRefs graphNo

replaceClaferWikiBlocks (CodeBlock (_, [ "clafer", "mooviz" ], _) _) =  do
    wikiEnv <- get
    return $ renderAnalyzeWithClaferMooViz (we_fileName wikiEnv) (we_mooVizURL wikiEnv) (we_wikiURL wikiEnv)

replaceClaferWikiBlocks (CodeBlock (_, [ "clafer", "config" ], _) _) =  do
    wikiEnv <- get
    return $ renderConfigureWithClaferConfigurator (we_fileName wikiEnv) (we_configuratorURL wikiEnv) (we_wikiURL wikiEnv)

replaceClaferWikiBlocks (CodeBlock (_, [ "clafer", "ide" ], _) _) =  do
    wikiEnv <- get
    return $ renderAddOpenInIDE (we_fileName wikiEnv) (we_ideURL wikiEnv) (we_wikiURL wikiEnv)

replaceClaferWikiBlocks block = return block

-- new line builder
nl :: Builder
nl = singleton '\n'

renderLinks :: String -> Builder
renderLinks fileName =
    "<div><b>Module Downloads:</b> | <a href=\"/clafer/" <> nl <>
    fromString fileName <> nl <>
    ".cfr\">[.cfr]</a> | <a href=\"/clafer/" <> nl <>
    fromString fileName <> nl <>
    ".html\">[.html]</a> |</div><br>\n" <> nl

renderStats :: String -> Builder
renderStats stats =
    "<div><b>Module Statistics:</b> \n| " <> nl <>
    fromString (intercalate " | " (lines stats)) <> nl <>
    " |</div><br>\n" <> nl

renderGraphWithToggle :: String           -> String        -> Int      -> Builder
renderGraphWithToggle    svgGraphWithoutRefs svgGraphWithRefs graphNo   =
    "<div id=\"" <> renderGraphId False graphNo <> "\" style=\"display:block;width:100%;border:solid lightgray 1px;overflow-x:auto;\" ondblclick=\"" <> renderShowRefs graphNo <> "\">" <> nl <>
    fromString svgGraphWithoutRefs <> nl <>
    "</div>" <> nl <>
    "<div id=\"" <> renderGraphId True graphNo <>  "\" style=\"display:none;width:100%;border:solid lightgray 1px;overflow-x:auto;\" ondblclick=\"" <> renderHideRefs graphNo <> "\">" <> nl <>
    fromString svgGraphWithRefs <> nl <>
    "</div>" <> nl

renderGraph :: String  -> Builder
renderGraph    svgGraph = 
    "<div style=\"display:block;width:100%;border:solid lightgray 1px;overflow-x:auto;\">" <> nl <>
    fromString svgGraph <> nl <>
    "</div>" <> nl

renderShowRefs :: Int    -> Builder
renderShowRefs    graphNo =
  "var gwr=document.getElementById('" <> renderGraphId True graphNo <> "'); gwr.style.display='block'; gwr.scrollLeft=this.scrollLeft; this.style.display='none';"  <> nl

renderHideRefs :: Int    -> Builder
renderHideRefs    graphNo =
  "var gwor=document.getElementById('" <> renderGraphId False graphNo <>  "'); gwor.style.display='block'; gwor.scrollLeft=this.scrollLeft;this.style.display='none';" <> nl

renderGraphId :: Bool -> Int    -> Builder
renderGraphId    True    graphNo = fromString $ "graphWithRefs" ++ show graphNo
renderGraphId    False   graphNo = fromString $ "graphWithoutRefs" ++ show graphNo

renderSummary :: String -> String -> String           -> String        -> Int    -> Builder
renderSummary    fileName  stats     svgGraphWithoutRefs svgGraphWithRefs graphNo =
    renderGraphWithToggle svgGraphWithoutRefs svgGraphWithRefs graphNo <>
    renderStats stats <>
    renderLinks fileName

compileFragments :: [String] -> [ClaferMode] -> Either [ClaferErr] (Map.Map ClaferMode CompilerResult)
compileFragments    fragments   claferModes   =
    -- compile all clafer code
    runClafer defaultClaferArgs{
                mode=claferModes,
                keep_unused=True,
                add_comments=True,
                show_references=False } $
        do
            mapM_ addModuleFragment fragments
            parse
            iModule <- desugar Nothing
            compile iModule
            generate

renderAnalyzeWithClaferMooViz :: String -> String -> String -> Block
renderAnalyzeWithClaferMooViz fileName toolURL' wikiURL =
    mkHtmlBlock 
      ("<div>" <> nl <>
      "<a href=\"" <> fromString (escapeAttribute toolURL') <> "?claferFileURL=" <> fromString (escapeAttribute wikiURL) <> "clafer/" <> nl <>
      fromString fileName <> nl <>
      ".cfr\" target=\"_blank\" " <> nl <>
      "style=\"background-color: #ccc;color: white;text-decoration: none;padding: 1px 5px 1px 5px;\" >" <> nl <>
      "Analyze with ClaferMooVisualizer" <> nl <>
      "</a></div><br>" <> nl :: Builder)

renderConfigureWithClaferConfigurator :: String -> String -> String -> Block
renderConfigureWithClaferConfigurator fileName toolURL' wikiURL =
    mkHtmlBlock 
      ("<div>" <> nl <>
      "<a href=\"" <> fromString (escapeAttribute toolURL') <> "?claferFileURL=" <> fromString (escapeAttribute wikiURL) <> "clafer/" <> nl <>
      fromString fileName <> nl <>
      ".cfr\" target=\"_blank\" " <> nl <>
      "style=\"background-color: #ccc;color: white;text-decoration: none;padding: 1px 5px 1px 5px;\" >" <> nl <>
      "Configure with ClaferConfigurator" <> nl <>
      "</a></div><br>\n" :: Builder)

renderAddOpenInIDE :: String -> String -> String -> Block
renderAddOpenInIDE    fileName  toolURL' wikiURL =
    mkHtmlBlock 
      ("<div>" <> nl <>
      "<a href=\"" <> fromString (escapeAttribute toolURL') <> "?claferFileURL=" <> fromString (escapeAttribute wikiURL) <> "clafer/" <> nl <>
      fromString fileName <> nl <>
      ".cfr\" target=\"_blank\" " <> nl <>
      "style=\"background-color: #ccc;color: white;text-decoration: none;padding: 1px 5px 1px 5px;\" >" <> nl <>
      "Open in ClaferIDE" <> nl <>
      "</a></div><br>\n" :: Builder)

-- | The wiki as the browser addressed it, so that the links to the web tools and to the model
--   work from the client.
data WikiAddress = WikiAddress {
                    wa_scheme :: String,
                    -- | the host and the port, e.g., wiselab.uwaterloo.ca or localhost:8091
                    wa_authority :: String,
                    -- | the host without the port
                    wa_host :: String,
                    -- | e.g., https://wiselab.uwaterloo.ca/modelwiki/ or http://localhost:8091/
                    wa_wikiURL :: String
               }

-- | The host and the port from the X-Forwarded-Host header (behind a proxy) or the Host header
--   of the request; without either, the name of the machine and the configured port
--   (getHostName gives only the short name of the machine or of its container, e.g., t3-necsis
--   instead of t3-necsis.cs.uwaterloo.ca). Behind a reverse proxy, X-Forwarded-Proto gives the
--   scheme (e.g., https) and X-Forwarded-Prefix the path of the wiki (e.g., /modelwiki).
wikiAddress :: String -> String -> Request -> WikiAddress
wikiAddress hostName port request =
    WikiAddress scheme authority (hostOf authority) (scheme ++ "://" ++ authority ++ prefix ++ "/")
  where
    firstValue name = case trim . takeWhile (/= ',') . toString <$> getHeader name request of
        Just value | not (null value) -> Just value
        _                             -> Nothing
    authority = fromMaybe (hostName ++ ":" ++ port) (firstValue "x-forwarded-host" <|> firstValue "host")
    scheme = case firstValue "x-forwarded-proto" of
        Just "https" -> "https"
        _            -> "http"
    prefix = case dropWhileEnd (== '/') <$> firstValue "x-forwarded-prefix" of
        Just path@('/':_) -> path
        Just path         -> '/' : path
        Nothing           -> ""
    -- "host:port", "host", "[::1]:port", or "[::1]"
    hostOf host = case break (== ':') (reverse host) of
        (revPort@(_:_), ':' : revHost) | all isDigit revPort -> reverse revHost
        _                                                    -> host
    trim = dropWhileEnd isSpace . dropWhile isSpace

-- | The URL of a web tool, ending with /: from its environment variable, a URL or a path on the
--   wiki's host (e.g., /claferide/ behind a reverse proxy); by default, the tool's port on the
--   wiki's host.
toolURL :: WikiAddress -> String -> Maybe String -> String
toolURL address defaultPort setting = withSlash $ case setting of
    Just path@('/':_)              -> wa_scheme address ++ "://" ++ wa_authority address ++ path
    Just url | not (null url)      -> url
    _                              -> wa_scheme address ++ "://" ++ wa_host address ++ ":" ++ defaultPort ++ "/"
  where
    withSlash url = if "/" `isSuffixOf` url then url else url ++ "/"

-- | The headers come from the client, so they must not end an attribute.
escapeAttribute :: String -> String
escapeAttribute = concatMap escape
  where
    escape '"' = "&quot;"
    escape '&' = "&amp;"
    escape '<' = "&lt;"
    escape '>' = "&gt;"
    escape c   = [c]

getPageName:: PluginM String
getPageName = replace " " "_" . replace "/" "_" . pgPageName . ctxLayout <$> getContext

-- | dot's SVG output starts with an XML declaration, a DOCTYPE and comments, which
-- do not belong inside an HTML page (and HTML rewriting proxies mangle them);
-- keep only the svg element.
inlineSvg :: String -> String
inlineSvg svg = case filter ("<svg" `isPrefixOf`) (tails svg) of
    svgElement : _ -> svgElement
    []             -> svg

changeTransparentToLightGray :: String -> String
changeTransparentToLightGray = replace "color=transparent" "color=lightgray"
