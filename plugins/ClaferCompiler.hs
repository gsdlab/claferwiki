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
import Data.Maybe (fromJust)
import Language.Clafer
import Language.Clafer.ClaferArgs
import Language.Clafer.Css
-- import Data.List (intercalate)

plugin :: Plugin
plugin = mkPageTransformM callClafer

callClafer :: Block -> PluginM Block
callClafer (CodeBlock (id, classes, namevals) contents)
  | first classes == "clafer" = liftIO $ do
  notCompiled <- doesFileExist "static/clafer/temp.txt"
  if notCompiled
     then do model <- readFile "static/clafer/temp.txt"
             compileFragments defaultClaferArgs{mode=Just Html, keep_unused=Just True} model
             return (CodeBlock (id, classes, namevals) contents)
     else return (CodeBlock (id, classes, namevals) contents)
callClafer x = return x

--compileFragments :: ClaferArgs -> [InputModel] -> Either ClaferErr CompilerResult
compileFragments args model =
    runClaferT args $
      do
        let name = uniqueName model
        addFragment $ fragments model
        parse
        compile
        CompilerResult {extension = ext,
                        outputCode = output,
                        statistics = stats} <- generateHtml
        liftIO $ do
          writeFile ("static/clafer/" ++ name ++ "." ++ ext)
                    ("<head><link rel=\"stylesheet\" type=\"text/css\" href=\"../css/custom.css\" /></head>\n" ++ output)
          writeFile "static/clafer/output.html" $ output ++ "\n<!-- # FRAGMENT -->"
          writeFile "static/clafer/name.txt" name
          writeFile ("static/clafer/" ++ name ++ ".cfr") model
          removeFile "static/clafer/temp.txt"
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
