module Network.Gitit.Plugin.ClaferWiki (plugin) where

import Network.Gitit.Interface
import Data.List.Split
import Language.Clafer

plugin :: Plugin
plugin = mkPageTransformM claferWiki

claferWiki :: Pandoc -> PluginM Pandoc
claferWiki (Pandoc meta blocks) = do
	-- collect clafer model fragments
	let
		fragments = filter (not . null) $ map addFragment blocks
		htmlCodeFragments = splitOn "\n<!-- # FRAGMENT /-->\n" $ compileFragments fragments
		newBlocks = replaceClaferBlocks htmlCodeFragments blocks
	return $ Pandoc meta newBlocks
	where
		addFragment :: Block -> String
		addFragment (CodeBlock (_, [ "clafer" ], _) code) = code ++ "\n"
		addFragment _ = ""

replaceClaferBlocks :: [ String ] -> [ Block ]  -> [ Block ]
replaceClaferBlocks (fragment:fragments) ((CodeBlock (_, [ "clafer" ], _) _):blocks) = 
	(RawBlock "html" ("<div class=\"code\">" ++ fragment ++ "</div>")) : replaceClaferBlocks fragments blocks
replaceClaferBlocks fragments (block:blocks) = block : replaceClaferBlocks fragments blocks
replaceClaferBlocks _ [] = []


compileFragments :: [ String ] -> String
compileFragments fragments = 
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
		extractOutput _	= ""