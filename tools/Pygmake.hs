import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception (Exception, throw)
import System.Environment
import System.Exit
import System.Process

import Pygmalion.Analyze
import Pygmalion.Core
import Pygmalion.Database
import Pygmalion.JSON
import Pygmalion.RPC.Server

main :: IO ()
main = do
  args <- (getArgs >>= parseArgs)
  ensureDB dbFile
  port <- newEmptyMVar
  chan <- newChan
  withAsync (runAnalysisThread chan) $ \analysis -> do
    ensureSuccess =<< (race (runRPCServer port chan) (executeMake port args))
    writeChan chan Nothing  -- Signifies end of data.
    ensureNoException =<< waitCatch analysis
  writeCompileCommands

usage :: IO ()
usage = putStrLn $ "Usage: " ++ makeExecutable ++ " [make arguments]"

parseArgs :: [String] -> IO [String]
parseArgs ["--help"] = usage >> exitSuccess
parseArgs ["-h"]     = usage >> exitSuccess
parseArgs as         = return as

executeMake:: MVar Int -> [String] -> IO ExitCode
executeMake port as = do
    portNumber <- readMVar port
    (_, _, _, handle) <- createProcess $ proc "make" (newArgs (show portNumber))
    waitForProcess handle
  where
    newArgs p = [cc p, cxx p] ++ as
    cc p = "CC=" ++ (callPygscan p) ++ clangExecutable
    cxx p = "CXX=" ++ (callPygscan p) ++ clangppExecutable
    callPygscan p = scanExecutable ++ " --make " ++ p ++ " "

ensureSuccess :: Either () ExitCode -> IO ()
ensureSuccess (Right code@(ExitFailure _)) = exitWith code
ensureSuccess (Right _)                    = return ()
ensureSuccess _                            = error "RPC server terminated early"

ensureNoException :: Exception a => Either a b -> IO b
ensureNoException (Right v) = return v
ensureNoException (Left e)  = putStrLn "Analysis thread threw" >> throw e

writeCompileCommands :: IO ()
writeCompileCommands = withDB dbFile $ \h -> do
  getAllRecords h >>= (writeFile compileCommandsFile) . sourceRecordsToJSON
