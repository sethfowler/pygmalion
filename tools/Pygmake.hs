import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception (Exception, throw)
import System.Environment
import System.Exit
import System.Process

import Pygmalion.Analyze
import Pygmalion.Core
import Pygmalion.Config
import Pygmalion.Database
import Pygmalion.JSON
import Pygmalion.RPC.Server

main :: IO ()
main = do
  args <- (getArgs >>= parseArgs)
  ensureDB dbFile
  cf <- getConfiguration
  port <- newEmptyMVar
  chan <- newChan
  withAsync (runAnalysisThread chan) $ \analysis -> do
    ensureSuccess =<< (race (runRPCServer cf port chan) (executeMake cf port args))
    writeChan chan Nothing  -- Signifies end of data.
    ensureNoException =<< waitCatch analysis
  writeCompileCommands

usage :: IO ()
usage = putStrLn $ "Usage: " ++ makeExecutable ++ " [make arguments]"

parseArgs :: [String] -> IO [String]
parseArgs ["--help"] = usage >> exitSuccess
parseArgs ["-h"]     = usage >> exitSuccess
parseArgs as         = return as

executeMake:: Config -> MVar Int -> [String] -> IO ExitCode
executeMake cf port as = do
    portNumber <- readMVar port
    (_, _, _, handle) <- createProcess $ proc (make cf) (newArgs (show portNumber))
    waitForProcess handle
  where
    newArgs p = [setCC p, setCXX p] ++ as
    setCC p = "CC=" ++ (callPygscan p) ++ (cc cf)
    setCXX p = "CXX=" ++ (callPygscan p) ++ (cpp cf)
    callPygscan p = scanExecutable ++ " --make " ++ p ++ " "

ensureSuccess :: Either () ExitCode -> IO ()
ensureSuccess (Right code@(ExitFailure _)) = exitWith code
ensureSuccess (Right _)                    = return ()
ensureSuccess _                            = error "RPC server terminated early"

ensureNoException :: Exception a => Either a b -> IO b
ensureNoException (Right v) = return v
ensureNoException (Left e)  = putStrLn "Analysis thread threw an exception" >> throw e

writeCompileCommands :: IO ()
writeCompileCommands = withDB dbFile $ \h -> do
  getAllRecords h >>= (writeFile compileCommandsFile) . sourceRecordsToJSON
