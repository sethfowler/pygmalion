import Control.Concurrent.Async
import Control.Monad
import Control.Monad.Trans
import qualified Data.Text as T
import System.Environment
import System.Exit
import System.Process

import Pygmalion.Analysis.Command
import Pygmalion.Core
import Pygmalion.RPC.Client

main :: IO ()
main = do
  (port, cmd) <- (getArgs >>= parseArgs)
  void $ concurrently (runCmd cmd) (sendScanMessageIfValid port cmd)

usage :: IO a
usage = putStrLn ("Usage: " ++ scanExecutable ++ " --make [port] [command]")
     >> exitWith (ExitFailure (-1))

die :: String -> ExitCode -> IO a
die s c = putStrLn (scanExecutable ++ ": " ++ s) >> exitWith c

needArg :: String -> IO a
needArg s = die ("No " ++ s ++ " specified") (ExitFailure (-1))

parseArgs :: [String] -> IO (Port, Command)
parseArgs ["--help"]                   = usage
parseArgs ["-h"]                       = usage
parseArgs ("--make" : port : cmd : as) = return (read port, Command (T.pack cmd) (map T.pack as))
parseArgs ("--make" : _ : [])          = needArg "command"
parseArgs ("--make" : [])              = needArg "port"
parseArgs _                            = usage

runCmd :: Command -> IO ()
runCmd (Command c as) = do
  (_, _, _, handle) <- liftIO $ createProcess (proc (T.unpack c) (map T.unpack as))
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
