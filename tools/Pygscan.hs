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

usage   = putStrLn $ "Usage: " ++ scanExecutable ++ " [database directory] [command]"
die s c = putStrLn (scanExecutable ++ ": " ++ s) >> exitWith c

parseArgs :: [String] -> IO (FilePath, Command)
parseArgs ["--help"]    = usage >> exitSuccess
parseArgs ["-h"]        = usage >> exitSuccess
parseArgs (dbPath : as) = return (dbPath, as)
parseArgs _             = usage >> exitSuccess

runCmd :: (FilePath, Command) -> IO (FilePath, Command)
runCmd (dbPath, cmd@(c : as)) = do
  (_, _, _, handle) <- createProcess (proc c as)
  code <- waitForProcess handle
  case code of
    ExitSuccess -> return (dbPath, cmd)
    _           -> die "Command failed" code

analyzeCmd :: (FilePath, Command) -> IO (FilePath, CommandInfo)
analyzeCmd (dbPath, cmd) = do
  mci <- getCommandInfo cmd
  case mci of
    Just ci -> return (dbPath, ci)
    _       -> exitSuccess

analyzeCode :: (FilePath, CommandInfo) -> IO (FilePath, CommandInfo, [FilePath])
analyzeCode (dbPath, ci) = do
  includedFiles <- clangGetIncludes ci
  let localHeaders = filter isLocalHeader includedFiles
  return (dbPath, ci, map normalise localHeaders)

updateDB :: (FilePath, CommandInfo, [FilePath]) -> IO ()
updateDB (dbPath, ci, headers) = withDB dbPath $ \handle -> do
  updateRecord handle ci
  -- Add entries for all included non-system headers, using the same metadata.
  forM_ headers $ \header -> do
    updateRecord handle $ updateSourceFile ci header
