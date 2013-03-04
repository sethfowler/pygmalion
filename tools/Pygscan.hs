import Control.Monad.Trans
import System.Environment
import System.Exit
import System.Process

import Pygmalion.Analyze
import Pygmalion.Core

main :: IO ()
main = getArgs
   >>= parseArgs
   >>= (\a@(_, cmd) -> runCmd cmd >> scanCommandAndUpdateDB a)

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

runCmd :: Command -> IO ()
runCmd (Command c as) = do
  (_, _, _, handle) <- liftIO $ createProcess (proc c as)
  code <- liftIO $ waitForProcess handle
  case code of
    ExitSuccess -> return ()
    _           -> liftIO $ die "Command failed" code
