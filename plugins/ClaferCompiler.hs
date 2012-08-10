module ClaferCompiler (plugin) where

import Network.Gitit.Interface
import System.Directory (doesFileExist, removeFile)
import Control.Monad.Trans (liftIO)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(ExitSuccess))
-- from the utf8-string package on HackageDB:
import Data.ByteString.Lazy.UTF8 (fromString)
-- from the SHA package on HackageDB:
import Data.Digest.Pure.SHA (sha1, showDigest)
import System.FilePath.Posix
import Data.Maybe (fromJust)
import Language.Clafer
import Language.ClaferT
import Language.Clafer.ClaferArgs
import Language.Clafer.Css
import Data.List (genericSplitAt)

plugin :: Plugin
plugin = mkPageTransformM callClafer

callClafer :: Block -> PluginM Block
callClafer (CodeBlock (id, classes, namevals) contents)
  | first classes == "clafer" = liftIO $ do
  notCompiled <- doesFileExist "static/clafer/temp.txt"
  if notCompiled
     then do model <- readFile "static/clafer/temp.txt"
             catch (compileFragments defaultClaferArgs{mode=Just Html, keep_unused=Just True} model)  model
             return (CodeBlock (id, classes, namevals) contents)
     else return (CodeBlock (id, classes, namevals) contents)
  where
    catch (Right (CompilerResult{outputCode = output})) model = do
          let name = uniqueName model
          writeFile ("static/clafer/" ++ name ++ ".html")
                    (header ++ css ++ "</head>\n<body>\n" ++ output ++ "</body>\n</html>")
          writeFile "static/clafer/output.html" $ output ++ "\n<!-- # FRAGMENT /-->"
          writeFile "static/clafer/name.txt" name
          writeFile ("static/clafer/" ++ name ++ ".cfr") model
          removeFile "static/clafer/temp.txt"
    catch (Left err) model = do
          let name = uniqueName model
          let output = highlightErrors model err
          writeFile ("static/clafer/" ++ name ++ ".html") (header ++ css ++ "</head>\n<body>\n" ++ output ++ "</body>\n</html>")
          writeFile "static/clafer/output.html" $ output ++ "\n<!-- # FRAGMENT /-->"
          writeFile "static/clafer/name.txt" name
          writeFile ("static/clafer/" ++ name ++ ".cfr") model
          removeFile "static/clafer/temp.txt"
    highlightErrors :: String -> [ClaferErr] -> String
    highlightErrors model errors = "<pre>\n" ++ unlines (replace "<!-- # FRAGMENT /-->" "</pre>\n<!-- # FRAGMENT /-->\n<pre>" --assumes the fragments have been concatenated
                                                          (highlightErrors' (replace "//# FRAGMENT" "<!-- # FRAGMENT /-->" (lines model)) errors)) ++ "</pre>"
    highlightErrors' :: [String] -> [ClaferErr] -> [String]
    highlightErrors' model [] = model
    highlightErrors' model ((ClaferErr msg):es) = highlightErrors' model es
    highlightErrors' model ((ParseErr ErrPos{modelPos = Pos l c, fragId = n} msg):es) = do
      let (ls, lss) = genericSplitAt (l + toInteger n) model
      let newLine = fst (genericSplitAt (c - 1) $ last ls) ++ "<span class=\"error\" title=\"Parse failed at line " ++ show l ++ " column " ++ show c ++
                                                                "...\n" ++ msg ++ "\">" ++ snd (genericSplitAt (c - 1) $ last ls) ++ "</span>"
      highlightErrors' (init ls ++ [newLine] ++ lss) es
    replace x y []     = []
    replace x y (z:zs) = (if x == z then y else z):replace x y zs
callClafer x = return x

compileFragments :: ClaferArgs -> InputModel -> Either [ClaferErr] CompilerResult
compileFragments args model =
  do
   result <- runClafer args $
              do
               let name = uniqueName model
               addFragment $ fragments model
               parse
               compile
               generate
   return result
      where
        addFragment []     = return ()
        addFragment (x:xs) = addModuleFragment x >> addFragment xs
        fragments model = map unlines $ fragments' $ lines model
        fragments' []                  = []
        fragments' ("//# FRAGMENT":xs) = fragments' xs
        fragments' model               = takeWhile (/= "//# FRAGMENT") model : fragments' (dropWhile (/= "//# FRAGMENT") model)

-- this is added so that it won't break if the wiki contains code blocks with no headers
first [] = []
first (x:xs) = x

-- | Generate a unique filename given the file's contents.
uniqueName :: String -> String
uniqueName = showDigest . sha1 . fromString
