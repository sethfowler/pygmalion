import Control.Exception (catch, SomeException)
import Data.String.Utils
import System.Environment
import System.Exit
import System.Process

import Pygmalion.Core
import Pygmalion.Config
import Pygmalion.Log
import Pygmalion.RPC.Client

main :: IO ()
main = do
  cf <- getConfiguration
  initLogger (logLevel cf)
  args <- (getArgs >>= parseArgs)

  -- Make sure pygd is running.
  (withRPC cf $ runRPC rpcPing) `catch` handleRPCFailure

  executeMake cf args

usage :: IO a
usage = do
  putStrLn $ "Usage: " ++ makeExecutable ++ " [make arguments]"
  exitWith (ExitFailure (-1))

parseArgs :: [String] -> IO [String]
parseArgs ["--help"] = usage
parseArgs ["-h"]     = usage
parseArgs as         = return as

getMakeCommand :: Config -> [String] -> String
getMakeCommand cf mkArgs = replace "$(idx)" queryExecutable
                         . replace "$(idxargs)" "make"
                         . replace "$(cc)" (ccCmd cf)
                         . replace "$(ccargs)" (ccArgs cf)
                         . replace "$(cpp)" (cppCmd cf)
                         . replace "$(cppargs)" (cppArgs cf)
                         . replace "$(makeargs)" (makeArgs cf ++ (join " " mkArgs))
                         . makeCmd $ cf

executeMake :: Config -> [String] -> IO ()
executeMake cf mkArgs = do
    (_, _, _, handle) <- createProcess . shell $ getMakeCommand cf mkArgs
    ensureSuccess =<< waitForProcess handle

handleRPCFailure :: SomeException -> IO ()
handleRPCFailure _ = putStrLn "Can't connect to pygd. Build information will not be recorded."

ensureSuccess :: ExitCode -> IO ()
ensureSuccess code@(ExitFailure _) = exitWith code
ensureSuccess _                    = return ()
