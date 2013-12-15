module Network.Gitit.Plugin.ClaferWiki (plugin) where

import Network.Gitit.Interface

import Control.Monad (when)
import Data.List
import Data.List.Split
import Data.Maybe
import Data.String.Utils (replace)
import Network.BSD (getHostName)
import System.Directory (createDirectoryIfMissing, doesFileExist)

import Language.Clafer
import Language.Clafer.Css 
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
		fragments :: [ String ]
		fragments = mapMaybe addFragment blocks
		completeModel = concat fragments
		fragmentedModel = intercalate "//# FRAGMENT\n" fragments
--		modes :: [ ClaferMode ]
--		modes = mapMaybe addMode blocks
		htmlCode = compileFragments fragments fragmentedModel
		htmlCodeFragments = splitOn "\n<!-- # FRAGMENT /-->\n" htmlCode
		newBlocks = replaceClaferWikiBlocks pageName serverURL serverPort  htmlCodeFragments blocks

	-- save original model
	liftIO $ writeFile ("static/clafer/" ++ pageName ++ ".cfr") completeModel
	-- save html version
	liftIO $ writeFile ("static/clafer/" ++ pageName ++ ".html") htmlCode

	return $ Pandoc meta newBlocks
	where
		addFragment :: Block -> Maybe String
		addFragment (CodeBlock (_, [ "clafer" ], _) code) = Just $ code ++ "\n"
		addFragment _                                     = Nothing

		addMode :: Block -> Maybe ClaferMode
		addMode (CodeBlock (_, [ "clafer" ], _) _) = Just Html
		addMode (CodeBlock (_, [ "clafer", "graph" ], _) _) = Just Graph
		addMode (CodeBlock (_, [ "clafer", "summary" ], _) _) = Just Graph
		addMode (CodeBlock (_, [ "clafer", "cvlgraph" ], _) _) = Just CVLGraph
		addMode _ = Nothing

replaceClaferWikiBlocks :: String -> String -> String -> [ String ] -> [ Block ]  -> [ Block ]
replaceClaferWikiBlocks fileName serverURL serverPort (fragment:fragments) ((CodeBlock (_, [ "clafer" ], _) _):blocks) = 
	(RawBlock "html" ("<div class=\"code\">" ++ fragment ++ "</div>")) : replaceClaferWikiBlocks fileName serverURL serverPort fragments blocks

replaceClaferWikiBlocks fileName serverURL serverPort fragments ((CodeBlock (_, [ "clafer", "links" ], _) _):blocks) = 
	(RawBlock "html" ("<div><b>Module Downloads:</b> | <a href=\"/clafer/" ++ fileName ++ ".cfr\">[.cfr]</a> | <a href=\"/clafer/" ++ fileName ++ ".html\">[.html]</a> |</div><br>\n")) : replaceClaferWikiBlocks fileName serverURL serverPort fragments blocks

replaceClaferWikiBlocks fileName serverURL serverPort fragments ((CodeBlock (_, [ "clafer", "stats" ], _) _):blocks) = 
	(RawBlock "html" ("<div>" ++ "Module statistics" ++ "</div>")) : replaceClaferWikiBlocks fileName serverURL serverPort fragments blocks

replaceClaferWikiBlocks fileName serverURL serverPort fragments ((CodeBlock (_, [ "clafer", "graph" ], _) _):blocks) = 
	(RawBlock "html" ("<div>" ++ "Graph" ++ "</div>")) : replaceClaferWikiBlocks fileName serverURL serverPort fragments blocks

replaceClaferWikiBlocks fileName serverURL serverPort fragments ((CodeBlock (_, [ "clafer", "cvlGraph" ], _) _):blocks) = 
	(RawBlock "html" ("<div>" ++ "CVLGraph" ++ "</div>")) : replaceClaferWikiBlocks fileName serverURL serverPort fragments blocks

replaceClaferWikiBlocks fileName serverURL serverPort fragments ((CodeBlock (_, [ "clafer", "summary" ], _) _):blocks) = 
	(RawBlock "html" ("<div>" ++ "summary goes here" ++ "</div>")) : replaceClaferWikiBlocks fileName serverURL serverPort fragments blocks

replaceClaferWikiBlocks fileName serverURL serverPort fragments ((CodeBlock (_, [ "clafer", "mooviz" ], _) _):blocks) = 
	(analyzeWithClaferMooViz fileName serverURL serverPort) : replaceClaferWikiBlocks fileName serverURL serverPort fragments blocks

replaceClaferWikiBlocks fileName serverURL serverPort fragments ((CodeBlock (_, [ "clafer", "ide" ], _) _):blocks) = 
	(addOpenInIDE fileName serverURL serverPort) : replaceClaferWikiBlocks fileName serverURL serverPort fragments blocks

replaceClaferWikiBlocks fileName serverURL serverPort fragments (block:blocks) = block : replaceClaferWikiBlocks fileName serverURL serverPort fragments blocks
replaceClaferWikiBlocks _ _ _ _ [] = []


compileFragments :: [ String ] -> String          -> String
compileFragments    fragments     fragmentedModel = 
	-- compile all clafer code
	let 
		htmlArgs = defaultClaferArgs{mode=[Html], keep_unused=True, add_comments=True}
		results = runClafer htmlArgs $ do
			mapM_ addModuleFragment $ fragments
			parse
			compile
			generate
	in 
		extractOutput results
	where
		extractOutput :: Either [ClaferErr] [CompilerResult] -> String
		extractOutput (Right ( [CompilerResult{outputCode = output} ])) = output
		extractOutput (Left err) = highlightErrors fragmentedModel err
		extractOutput _	= ""


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