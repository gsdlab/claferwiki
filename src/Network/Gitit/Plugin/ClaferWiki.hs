module Network.Gitit.Plugin.ClaferWiki (plugin) where

import Network.Gitit.Interface

import Control.Monad
import Control.Monad.Trans.State
import Data.List
import Data.List.Split
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Data.String.Utils (replace)
import Network.BSD (getHostName)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Process (readProcessWithExitCode)

import Language.Clafer
import Language.Clafer.Css as Css
import Language.Clafer.Generator.Html (highlightErrors)

plugin :: Plugin
plugin = mkPageTransformM claferWiki

-- claferWiki collects Clafer code from .clafer code blocks, renders as HTML and graph, 
-- and replaces the original blocks with RawBlocks containing the results
claferWiki :: Pandoc -> PluginM Pandoc
claferWiki pandoc@(Pandoc _ blocks) = do
	-- make sure the directories and clafer.css exist
	liftIO $ do 
		createDirectoryIfMissing True "static/clafer/"
		createDirectoryIfMissing True "static/css/"
		cssExist <- doesFileExist "static/css/clafer.css"
		unless cssExist $ writeFile "static/css/clafer.css" css

	serverURL <- liftIO $ getHostName
	pageName <- getPageName
	config <- askConfig
	
	let
		serverPort = show $ portNumber config
		-- collects compiler modes depending on the kinds of blocks on the page 
		claferModes :: [ ClaferMode ]
		claferModes = mapMaybe addMode blocks
		-- produce the required outputs in a single compilation
		allCompilationResults = compileFragments fragments claferModes
		
		htmlCode = extractOutput allCompilationResults Html
		htmlCodeFragments = splitOn "\n<!-- # FRAGMENT /-->\n" htmlCode
		
		stats = case extractCompilerResult allCompilationResults Html of
					Just CompilerResult{statistics = s} -> s
					Nothing 					   		-> "No model."

		dotGraph = case extractCompilerResult allCompilationResults Graph of
				Just CompilerResult{outputCode = o} -> o
				Nothing 							-> ""

		dotCVLGraph = case extractCompilerResult allCompilationResults CVLGraph of
					Just CompilerResult{outputCode = o} -> o
					Nothing 					    -> ""

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
							we_graphNo = 0,		-- needed to construct unique IDs of <div> for graphs
							we_svgGraphWithRefs = svgGraphWithRefs,
							we_svgGraphWithoutRefs = svgGraphWithoutRefs,
							we_svgCVLGraph = svgCVLGraph
						 }
		newPandoc = evalState (bottomUpM replaceClaferWikiBlocks pandoc) initialWikiEnv

	-- save original model
	liftIO $ writeFile ("static/clafer/" ++ pageName ++ ".cfr") completeModel
	-- save html version
	liftIO $ writeFile ("static/clafer/" ++ pageName ++ ".html") $ selfContained htmlCode

	return $ newPandoc
	where
		-- collect clafer model fragments
		fragments :: [ String ]
		fragments = queryWith addFragment pandoc
		fragmentedModel = intercalate "//# FRAGMENT\n" fragments
		completeModel = intercalate "\n" fragments
		
		addFragment :: Block -> [String]
		addFragment (CodeBlock (_, [ "clafer" ], _) code) = [ code ++ "\n" ]
		addFragment _                                     = []

		addMode :: Block -> Maybe ClaferMode
		addMode (CodeBlock (_, [ "clafer" ], _) _) = Just Html
		addMode (CodeBlock (_, [ "clafer", "graph" ], _) _) = Just Graph
		addMode (CodeBlock (_, [ "clafer", "summary" ], _) _) = Just Graph
		addMode (CodeBlock (_, [ "clafer", "cvlGraph" ], _) _) = Just CVLGraph
		addMode (CodeBlock (_, [ "clafer", "cvlgraph" ], _) _) = Just CVLGraph
		addMode _ = Nothing


		extractCompilerResult :: Either [ClaferErr] (Map.Map ClaferMode CompilerResult) -> ClaferMode -> Maybe CompilerResult
		extractCompilerResult (Right compilerResultMap) claferMode = Map.lookup claferMode compilerResultMap
		extractCompilerResult (Left _)                  _          = Nothing

		extractOutput :: Either [ClaferErr] (Map.Map ClaferMode CompilerResult) -> ClaferMode -> String
		extractOutput (Right compilerResultMap) claferMode = 
			case (Map.lookup claferMode compilerResultMap) of
				Just CompilerResult{outputCode = output} -> output
				Nothing -> "Error: No " ++ show claferMode ++ " output!"
		extractOutput (Left err) _ = highlightErrors fragmentedModel err
		extractOutput _	_ = ""

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

data WikiEnv = WikiEnv {
					we_fileName :: String,
					we_serverURL :: String,
					we_serverPort :: String, 
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
	let (fragment:fragments) = we_htmlCodeFragments wikiEnv
	put $ wikiEnv { we_htmlCodeFragments = fragments }
	return $ RawBlock "html" ("<div class=\"code\">" ++ fragment ++ "</div>")

replaceClaferWikiBlocks (CodeBlock (_, [ "clafer", "links" ], _) _) = do
	fileName <- gets we_fileName
	return $ RawBlock "html" $ renderLinks fileName
		
replaceClaferWikiBlocks (CodeBlock (_, [ "clafer", "stats" ], _) _) = do
	stats <- gets we_stats
	return $ RawBlock "html" $ renderStats stats

replaceClaferWikiBlocks (CodeBlock (_, [ "clafer", "graph" ], _) _) = do
	wikiEnv <- get 
	graphNo <- gets we_graphNo
	svgGraphWithRefs <- gets we_svgGraphWithRefs
	svgGraphWithoutRefs <- gets we_svgGraphWithoutRefs
	put $ wikiEnv { we_graphNo = graphNo + 1 }
	return $ RawBlock "html" $ renderGraphWithToggle svgGraphWithoutRefs svgGraphWithRefs  graphNo

replaceClaferWikiBlocks (CodeBlock (_, [ "clafer", "cvlGraph" ], _) _) =  do
	wikiEnv <- get 
	graphNo <- gets we_graphNo
	svgCVLGraph <- gets we_svgCVLGraph
	put $ wikiEnv { we_graphNo = graphNo + 1 }
	return $ RawBlock "html" $ renderGraph svgCVLGraph

replaceClaferWikiBlocks (CodeBlock (_, [ "clafer", "cvlgraph" ], _) _) =  do
	wikiEnv <- get 
	graphNo <- gets we_graphNo
	svgCVLGraph <- gets we_svgCVLGraph
	put $ wikiEnv { we_graphNo = graphNo + 1 }
	return $ RawBlock "html" $ renderGraph svgCVLGraph

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

renderLinks :: String -> String
renderLinks fileName = 
	"<div><b>Module Downloads:</b> | <a href=\"/clafer/" ++ 
	fileName ++ 
	".cfr\">[.cfr]</a> | <a href=\"/clafer/" ++ 
	fileName ++ 
	".html\">[.html]</a> |</div><br>\n"
	
renderStats :: String -> String
renderStats stats =
	"<div><b>Module Statistics:</b> \n| " ++ 
	(intercalate " | " $ lines stats ) ++ 
	" |</div><br>\n"


renderGraphWithToggle :: String           -> String        -> Int      -> String
renderGraphWithToggle    svgGraphWithoutRefs svgGraphWithRefs graphNo   = unlines [
    "<div id=\"" ++ renderGraphId False graphNo ++  "\" style=\"display:block;width:100%;border:solid lightgray 1px;overflow-x:auto;\" ondblclick=\"" ++ renderShowRefs graphNo ++ "\">",
    svgGraphWithoutRefs, 
    "</div>",
    "<div id=\"" ++ renderGraphId True graphNo ++  "\" style=\"display:none;width:100%;border:solid lightgray 1px;overflow-x:auto;\" ondblclick=\"" ++ renderHideRefs graphNo ++ "\">",
    svgGraphWithRefs, 
    "</div>" ]

renderGraph :: String  -> String
renderGraph    svgGraph = unlines [
    "<div style=\"display:block;width:100%;border:solid lightgray 1px;overflow-x:auto;\">",
    svgGraph, 
    "</div>" ]

renderShowRefs :: Int    -> String 
renderShowRefs    graphNo =
  "var gwr=document.getElementById('" ++ renderGraphId True graphNo ++  "'); gwr.style.display='block'; gwr.scrollLeft=this.scrollLeft; this.style.display='none';"

renderHideRefs :: Int    -> String
renderHideRefs    graphNo = 
  "var gwor=document.getElementById('" ++ renderGraphId False graphNo ++  "'); gwor.style.display='block'; gwor.scrollLeft=this.scrollLeft;this.style.display='none';"

renderGraphId :: Bool -> Int    -> String
renderGraphId    True    graphNo = "graphWithRefs" ++ show graphNo
renderGraphId    False   graphNo = "graphWithoutRefs" ++ show graphNo

renderSummary :: String -> String -> String           -> String        -> Int    -> String
renderSummary    fileName  stats     svgGraphWithoutRefs svgGraphWithRefs graphNo =
	renderGraphWithToggle svgGraphWithoutRefs svgGraphWithRefs graphNo ++ 
	renderStats stats ++
	renderLinks fileName


compileFragments :: [ String ] -> [ ClaferMode ] -> Either [ClaferErr] (Map.Map ClaferMode CompilerResult)
compileFragments    fragments     claferModes    = 
	let 
		noDuplicatesClaferModes =  Set.toList $ Set.fromList claferModes
	in
		-- compile all clafer code
		runClafer defaultClaferArgs{mode=noDuplicatesClaferModes, keep_unused=True, add_comments=True, show_references=False } $ do
				mapM_ addModuleFragment fragments
				parse
				compile
				generate

renderAnalyzeWithClaferMooViz :: String -> String -> String -> Block
renderAnalyzeWithClaferMooViz fileName serverURL serverPort = 
    RawBlock "html" (unlines [
      "<div>" ++
      "<a href=\"" ++ serverURL ++ ":8092/?claferFileURL=" ++ serverURL ++ ":" ++ serverPort ++ "/clafer/" ++ 
      fileName ++  
      ".cfr\" target=\"_blank\" " ++
      "style=\"background-color: #ccc;color: white;text-decoration: none;padding: 1px 5px 1px 5px;\" >" ++
      "Analyze with ClaferMooVisualizer" ++
      "</a></div><br>\n"
      ])

renderConfigureWithClaferConfigurator :: String -> String -> String -> Block
renderConfigureWithClaferConfigurator fileName serverURL serverPort = 
    RawBlock "html" (unlines [
      "<div>" ++
      "<a href=\"" ++ serverURL ++ ":8093/?claferFileURL=" ++ serverURL ++ ":" ++ serverPort ++ "/clafer/" ++ 
      fileName ++  
      ".cfr\" target=\"_blank\" " ++
      "style=\"background-color: #ccc;color: white;text-decoration: none;padding: 1px 5px 1px 5px;\" >" ++
      "Configure with ClaferConfigurator" ++
      "</a></div><br>\n"
      ])

renderAddOpenInIDE :: String -> String -> String -> Block
renderAddOpenInIDE fileName serverURL serverPort =
	RawBlock "html" (unlines [
      "<div>" ++
      "<a href=\"" ++ serverURL ++ ":8094/?claferFileURL=" ++ serverURL ++ ":" ++ serverPort ++ "/clafer/" ++ 
      fileName ++  
      ".cfr\" target=\"_blank\" " ++
      "style=\"background-color: #ccc;color: white;text-decoration: none;padding: 1px 5px 1px 5px;\" >" ++
      "Open in ClaferIDE" ++
      "</a></div><br>\n"
      ])

getPageName:: PluginM String
getPageName = getContext >>= return . replace " " "_" . replace "/" "_" . pgPageName . ctxLayout

changeTransparentToLightGray :: String -> String
changeTransparentToLightGray = replace "transparent" "lightgray"