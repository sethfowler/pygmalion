import Control.Monad
import Control.Monad.Reader
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
   >>= runScanner (\c -> runCmd c
                     >>= analyzeCmd
                     >>= analyzeCode
                     >>= updateDB)

type DBPath  = FilePath
type Scanner a = ReaderT DBPath IO a

runScanner :: (a -> Scanner b) -> (DBPath, a) -> IO b
runScanner f (db, v) = runReaderT (f v) db

usage :: IO ()
usage = putStrLn $ "Usage: " ++ scanExecutable ++ " [database directory] [command]"

die :: String -> ExitCode -> IO a
die s c = putStrLn (scanExecutable ++ ": " ++ s) >> exitWith c

parseArgs :: [String] -> IO (DBPath, Command)
parseArgs ["--help"]      = usage >> exitSuccess
parseArgs ["-h"]          = usage >> exitSuccess
parseArgs (db : cmd : as) = return (db, Command cmd as)
parseArgs (_ : [])        = die "No command specified" (ExitFailure (-1))
parseArgs _               = usage >> exitSuccess

runCmd :: Command -> Scanner Command
runCmd cmd@(Command c as) = do
  (_, _, _, handle) <- liftIO $ createProcess (proc c as)
  code <- liftIO $ waitForProcess handle
  case code of
    ExitSuccess -> return cmd
    _           -> liftIO $ die "Command failed" code

analyzeCmd :: Command -> Scanner CommandInfo
analyzeCmd cmd = do
  mci <- liftIO $ getCommandInfo cmd
  case mci of
    Just ci -> return ci
    _       -> liftIO $ exitSuccess

analyzeCode :: CommandInfo -> Scanner (CommandInfo, [FilePath])
analyzeCode ci = do
  includedFiles <- liftIO $ clangGetIncludes ci
  let localHeaders = filter isLocalHeader includedFiles
  return (ci, map normalise localHeaders)

updateDB :: (CommandInfo, [FilePath]) -> Scanner ()
updateDB (ci, headers) = do
  db <- ask
  liftIO $ withDB db $ \handle -> do
    updateRecord handle ci
    -- Add entries for all included non-system headers, using the same metadata.
    forM_ headers $ \header -> do
      updateRecord handle $ updateSourceFile ci header
