module Network.Gitit.Plugin.ClaferWiki (plugin) where

import Network.Gitit.Interface

import Control.Monad
import Control.Monad.Trans.State
import Data.List
import Data.List.Split
import qualified Data.Map as Map
import Data.Maybe
import Data.String.Utils (replace)
import Network.BSD (getHostName)
import System.Directory (createDirectoryIfMissing, doesFileExist)

import Language.Clafer
import Language.Clafer.Css as Css
import Language.Clafer.Generator.Html (highlightErrors)

plugin :: Plugin
plugin = mkPageTransformM claferWiki

claferWiki :: Pandoc -> PluginM Pandoc
claferWiki (Pandoc meta blocks) = do
	-- make sure the directories and clafer.css exist
	liftIO $ do 
		createDirectoryIfMissing True "static/clafer/"
		createDirectoryIfMissing True "static/css/"
		cssExist <- doesFileExist "static/css/clafer.css"
		when cssExist $ writeFile "static/css/clafer.css" css
	-- collect clafer model fragments

	serverURL <- liftIO $ getHostName
	pageName <- getPageName
	config <- askConfig
	
	let
		serverPort = show $ portNumber config
		claferModes :: [ ClaferMode ]
		claferModes = mapMaybe addMode blocks
		
		allCompilationResults = compileFragments fragments claferModes
		
		htmlCode = extractOutput allCompilationResults Html
		htmlCodeFragments = splitOn "\n<!-- # FRAGMENT /-->\n" htmlCode
		
		stats = case extractCompilerResult allCompilationResults Html of
					Just CompilerResult{statistics = s} -> s
					Nothing 					   		-> ""

		initialWikiEnv = WikiEnv {
							we_fileName = pageName,
							we_serverURL = serverURL,
							we_serverPort = serverPort, 
							we_fragments = fragments,
							we_stats = stats
						 }
		newBlocks = evalState (mapM replaceClaferWikiBlocks blocks) initialWikiEnv

	-- save original model
	liftIO $ writeFile ("static/clafer/" ++ pageName ++ ".cfr") completeModel
	-- save html version
	liftIO $ writeFile ("static/clafer/" ++ pageName ++ ".html") $ selfContained htmlCode

	return $ Pandoc meta newBlocks
	where
		fragments :: [ String ]
		fragments = mapMaybe addFragment blocks
		fragmentedModel = intercalate "//# FRAGMENT\n" fragments
		completeModel = intercalate "\n" fragments
		
		addFragment :: Block -> Maybe String
		addFragment (CodeBlock (_, [ "clafer" ], _) code) = Just $ code ++ "\n"
		addFragment _                                     = Nothing

		addMode :: Block -> Maybe ClaferMode
		addMode (CodeBlock (_, [ "clafer" ], _) _) = Just Html
		addMode (CodeBlock (_, [ "clafer", "graph" ], _) _) = Just Graph
		addMode (CodeBlock (_, [ "clafer", "summary" ], _) _) = Just Graph
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
					we_fragments :: [ String ],
					we_stats :: String
			   }



replaceClaferWikiBlocks :: Block -> State WikiEnv Block
replaceClaferWikiBlocks (CodeBlock (_, [ "clafer" ], _) _) = do
	wikiEnv <- get 
	let (fragment:fragments) = we_fragments wikiEnv
	put $ wikiEnv { we_fragments = fragments }
	return $ RawBlock "html" ("<div class=\"code\">" ++ fragment ++ "</div>")

replaceClaferWikiBlocks (CodeBlock (_, [ "clafer", "links" ], _) _) = do
	wikiEnv <- get 
	return $ RawBlock "html" (
			"<div><b>Module Downloads:</b> | <a href=\"/clafer/" ++ 
			(we_fileName wikiEnv) ++ 
			".cfr\">[.cfr]</a> | <a href=\"/clafer/" ++ 
			(we_fileName wikiEnv) ++ ".html\">[.html]</a> |</div><br>\n"
		)
		

replaceClaferWikiBlocks (CodeBlock (_, [ "clafer", "stats" ], _) _) = do
	wikiEnv <- get 
	return $ RawBlock "html" ("<div><b>Module Statistics:</b> \n| " ++ (intercalate " | " $ lines $ we_stats wikiEnv) ++ "|</div><br>\n")

replaceClaferWikiBlocks (CodeBlock (_, [ "clafer", "graph" ], _) _) = do
	wikiEnv <- get 
	return $ RawBlock "html" ("<div>" ++ "Graph" ++ "</div>")

replaceClaferWikiBlocks (CodeBlock (_, [ "clafer", "cvlGraph" ], _) _) =  do
	wikiEnv <- get 
	return $ RawBlock "html" ("<div>" ++ "CVLGraph" ++ "</div>")

replaceClaferWikiBlocks (CodeBlock (_, [ "clafer", "summary" ], _) _) =  do
	wikiEnv <- get 
	return $ RawBlock "html" ("<div>" ++ "Summary" ++ "</div>")

replaceClaferWikiBlocks (CodeBlock (_, [ "clafer", "mooviz" ], _) _) =  do
	wikiEnv <- get 
	return $ analyzeWithClaferMooViz (we_fileName wikiEnv) (we_serverURL wikiEnv) (we_serverPort wikiEnv)

replaceClaferWikiBlocks (CodeBlock (_, [ "clafer", "ide" ], _) _) =  do
	wikiEnv <- get 
	return $ addOpenInIDE (we_fileName wikiEnv) (we_serverURL wikiEnv) (we_serverPort wikiEnv)

replaceClaferWikiBlocks block = return block


compileFragments :: [ String ] -> [ ClaferMode ] -> Either [ClaferErr] (Map.Map ClaferMode CompilerResult)
compileFragments    fragments     claferModes    = 
	-- compile all clafer code
	runClafer defaultClaferArgs{mode=claferModes, keep_unused=True, add_comments=True } $ do
			mapM_ addModuleFragment fragments
			parse
			compile
			generate

analyzeWithClaferMooViz :: String -> String -> String -> Block
analyzeWithClaferMooViz fileName serverURL serverPort = 
    RawBlock "html" (unlines [
      "<div>" ++
      "<a href=\"" ++ serverURL ++ ":8092/?claferFileURL=" ++ serverURL ++ ":" ++ serverPort ++ "/clafer/" ++ 
      fileName ++  
      ".cfr\" target=\"_blank\" " ++
      "style=\"background-color: #ccc;color: white;text-decoration: none;padding: 1px 5px 1px 5px;\" >" ++
      "Analyze with ClaferMooVisualizer" ++
      "</a></div><br>\n"
      ])

addOpenInIDE :: String -> String -> String -> Block
addOpenInIDE fileName serverURL serverPort =
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