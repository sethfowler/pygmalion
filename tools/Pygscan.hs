import Control.Monad
import System.Environment
import System.Exit
import System.FilePath.Posix
import System.Process

import Pygmalion.Analyze.Command
import Pygmalion.Analyze.Source
import Pygmalion.Core
import Pygmalion.Database

main :: IO ()
main = getArgs
   >>= parseArgs
   >>= runCmd
   >>= analyzeCmd
   >>= analyzeCode
   >>= updateDB

usage :: IO ()
usage = putStrLn $ "Usage: " ++ scanExecutable ++ " [database directory] [command]"

die :: String -> ExitCode -> IO a
die s c = putStrLn (scanExecutable ++ ": " ++ s) >> exitWith c

parseArgs :: [String] -> IO (FilePath, Command)
parseArgs ["--help"]      = usage >> exitSuccess
parseArgs ["-h"]          = usage >> exitSuccess
parseArgs (db : cmd : as) = return (db, Command cmd as)
parseArgs (_ : [])        = die "No command specified" (ExitFailure (-1))
parseArgs _               = usage >> exitSuccess

runCmd :: (FilePath, Command) -> IO (FilePath, Command)
runCmd (db, cmd@(Command c as)) = do
  (_, _, _, handle) <- createProcess (proc c as)
  code <- waitForProcess handle
  case code of
    ExitSuccess -> return (db, cmd)
    _           -> die "Command failed" code

analyzeCmd :: (FilePath, Command) -> IO (FilePath, CommandInfo)
analyzeCmd (db, cmd) = do
  mci <- getCommandInfo cmd
  case mci of
    Just ci -> return (db, ci)
    _       -> exitSuccess

analyzeCode :: (FilePath, CommandInfo) -> IO (FilePath, CommandInfo, [FilePath])
analyzeCode (db, ci) = do
  includedFiles <- clangGetIncludes ci
  let localHeaders = filter isLocalHeader includedFiles
  return (db, ci, map normalise localHeaders)

updateDB :: (FilePath, CommandInfo, [FilePath]) -> IO ()
updateDB (db, ci, headers) = withDB db $ \handle -> do
  updateRecord handle ci
  -- Add entries for all included non-system headers, using the same metadata.
  forM_ headers $ \header -> do
    updateRecord handle $ updateSourceFile ci header
