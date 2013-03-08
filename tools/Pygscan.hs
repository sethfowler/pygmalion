import Control.Concurrent.Async
import Control.Monad
import Control.Monad.Trans
import System.Environment
import System.Exit
import System.Process

import Pygmalion.Analyze.Command
import Pygmalion.Core
import Pygmalion.RPC.Client

main :: IO ()
main = do
  (port, cmd) <- (getArgs >>= parseArgs)
  void $ concurrently (runCmd cmd) (sendScanMessageIfValid port cmd)

usage :: IO ()
usage = putStrLn $ "Usage: " ++ scanExecutable ++ " --make [port] [command]"

die :: String -> ExitCode -> IO a
die s c = putStrLn (scanExecutable ++ ": " ++ s) >> exitWith c

needArg :: String -> IO a
needArg s = die ("No " ++ s ++ " specified") (ExitFailure (-1))

parseArgs :: [String] -> IO (Port, Command)
parseArgs ["--help"]                   = usage >> exitSuccess
parseArgs ["-h"]                       = usage >> exitSuccess
parseArgs ("--make" : port : cmd : as) = return (read port, Command cmd as)
parseArgs ("--make" : _ : [])          = needArg "command"
parseArgs ("--make" : [])              = needArg "port"
parseArgs _                            = usage >> exitSuccess

runCmd :: Command -> IO ()
runCmd (Command c as) = do
  (_, _, _, handle) <- liftIO $ createProcess (proc c as)
  code <- liftIO $ waitForProcess handle
  case code of
    ExitSuccess -> return ()
    _           -> liftIO $ die "Command failed" code

sendScanMessageIfValid :: Port -> Command -> IO ()
sendScanMessageIfValid port cmd = do
  result <- getCommandInfo cmd
  case result of
    Just cmdInfo -> sendScanMessage port cmdInfo
    _            -> return ()   -- Can't do anything with this command.
