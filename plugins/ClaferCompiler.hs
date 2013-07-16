module ClaferCompiler (plugin) where

import Network.Gitit.Interface
import System.Directory (doesFileExist, removeFile)
-- import Control.Monad.Trans (liftIO)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(ExitSuccess))
-- from the utf8-string package on HackageDB:
import Data.ByteString.Lazy.UTF8 (fromString)
-- from the SHA package on HackageDB:
import Data.Digest.Pure.SHA (sha1, showDigest)
import System.FilePath
import Data.Maybe (fromJust)
import Language.Clafer
import Language.ClaferT
import Language.Clafer.ClaferArgs
import Language.Clafer.Css
import Language.Clafer.Generator.Html (highlightErrors)

plugin :: Plugin
plugin = mkPageTransformM callClafer

callClafer :: Block -> PluginM Block
callClafer (CodeBlock (id, classes, namevals) contents)
  | first classes == "clafer" = liftIO $ do
  notCompiled <- doesFileExist "static/clafer/temp.txt"
  if notCompiled
     then do model <- readFile "static/clafer/temp.txt"
             catch (compileFragments defaultClaferArgs{mode=Html, keep_unused=True, add_comments=True, noalloyruncommand=True} model)  model
             return (CodeBlock (id, classes, namevals) contents)
     else return (CodeBlock (id, classes, namevals) contents)
  where
    catch (Right (CompilerResult{outputCode = output})) model = do
          let name = uniqueName model
          writeFile ("static/clafer/" ++ name ++ ".html")
                    (header ++ "<style>" ++ css ++ "</style></head>\n<body>\n" ++ output ++ "</body>\n</html>")
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
