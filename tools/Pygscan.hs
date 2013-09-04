import Control.Concurrent.Async
import Control.Exception (catch, SomeException)
import Control.Monad
import Control.Monad.Trans
import System.Environment
import System.Exit
import System.Process

import Pygmalion.Core
import Pygmalion.Index.Command
import Pygmalion.RPC.Client

main :: IO ()
main = do
  (port, cmd, args) <- parseArgs =<< getArgs
  void $ concurrently (runCmd cmd args) (indexIfValid port cmd args)

usage :: IO a
usage = putStrLn ("Usage: " ++ scanExecutable ++ " --make [port] [command]")
     >> exitWith (ExitFailure (-1))

die :: String -> ExitCode -> IO a
die s c = putStrLn (scanExecutable ++ ": " ++ s) >> exitWith c

needArg :: String -> IO a
needArg s = die ("No " ++ s ++ " specified") (ExitFailure (-1))

parseArgs :: [String] -> IO (Port, String, [String])
parseArgs ["--help"]                     = usage
parseArgs ["-h"]                         = usage
parseArgs ("--make" : port : cmd : args) = return (read port, cmd, args)
parseArgs ("--make" : _ : [])            = needArg "command"
parseArgs ("--make" : [])                = needArg "port"
parseArgs _                              = usage

runCmd :: String -> [String] -> IO ()
runCmd cmd args = do
  (_, _, _, handle) <- createProcess (proc cmd args)
  code <- waitForProcess handle
  case code of
    ExitSuccess -> return ()
    _           -> liftIO $ die "Command failed" code

indexIfValid :: Port -> String -> [String] -> IO ()
indexIfValid port cmd args = do
  result <- getCommandInfo cmd args
  case result of
    Just ci -> (withRPCRaw port $ runRPC (rpcIndex ci)) `catch` handleRPCFailure
    _       -> return ()   -- Can't do anything with this command.

handleRPCFailure :: SomeException -> IO ()
handleRPCFailure _ = return ()  -- Silently ignore failure.
