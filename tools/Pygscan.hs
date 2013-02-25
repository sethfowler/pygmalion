import Control.Monad
import System.Environment
import System.Exit
import System.FilePath.Posix
import System.Process

import Pygmalion.Analyze.Command
import Pygmalion.Analyze.Source
import Pygmalion.Metadata
import Pygmalion.SourceDB

main = getArgs
   >>= parseArgs
   >>= runCmd
   >>= analyzeCmd
   >>= analyzeCode
   >>= updateDB

scanExecutable = "pygscan"

usage   = putStrLn $ "Usage: " ++ scanExecutable ++ " [command]"
die s c = putStrLn (scanExecutable ++ ": " ++ s) >> exitWith c

parseArgs :: [String] -> IO Command
parseArgs ["--help"] = usage >> exitSuccess
parseArgs ["-h"]     = usage >> exitSuccess
parseArgs []         = usage >> exitSuccess
parseArgs as         = return as

runCmd :: Command -> IO Command
runCmd cmd@(c : as) = do
  (_, _, _, handle) <- createProcess (proc c as)
  code <- waitForProcess handle
  case code of
    ExitSuccess -> return cmd
    _           -> die "Command failed" code

analyzeCmd :: Command -> IO CommandInfo
analyzeCmd cmd = do
  mci <- getCommandInfo cmd
  case mci of
    Just ci -> return ci
    _       -> exitSuccess

analyzeCode :: CommandInfo -> IO (CommandInfo, [FilePath])
analyzeCode ci = do
  includedFiles <- clangGetIncludes ci
  let localHeaders = filter isLocalHeader includedFiles
  return (ci, map normalise localHeaders)

updateDB :: (CommandInfo, [FilePath]) -> IO ()
updateDB (ci, headers) = withDB $ \handle -> do
  updateRecord handle ci
  -- Add entries for all included non-system headers, using the same metadata.
  forM_ headers $ \header -> do
    updateRecord handle $ updateSourceFile ci header
