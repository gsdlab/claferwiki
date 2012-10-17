module ClaferStats(plugin) where

import Network.Gitit.Interface
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(ExitSuccess))
-- from the utf8-string package on HackageDB:
import Data.ByteString.Lazy.UTF8 (fromString)
-- from the SHA package on HackageDB:
import Data.Digest.Pure.SHA (sha1, showDigest)
import System.FilePath ((</>))
import Control.Monad.Trans (liftIO)
import Language.Clafer

plugin :: Plugin
plugin = mkPageTransformM writeStats

writeStats :: Block -> PluginM Block
writeStats (CodeBlock (id, classes, namevals) contents)
  | "clafer" `elem` classes &&  "summary" `elem` classes = do
    cfg <- askConfig
    let model <- readFile "static/clafer/temp.txt";
        args = defaultClaferArgs{mode=Just Html, keep_unused=Just Graph};
        CompilerResult {outputCode = output statistics = stats} =
          generateM args (compileM args (addModuleFragment args model))
    liftIO $ do
    (ec, _out, err) <- readProcessWithExitCode "dot" ["-Tsvg", "-o",
                         staticDir cfg </> "img/" ++ outfile] contents
    if ec == ExitSuccess
       then return $ Para [Image name ("/img/" ++ outfile, ""), LineBreak, Str stats]
       else return $ Para [Str $ "dot returned an error status: " ++ err]
readBlock x = return x

-- | Generate a unique filename given the file's contents.
uniqueName :: String -> String
uniqueName = showDigest . sha1 . fromString
