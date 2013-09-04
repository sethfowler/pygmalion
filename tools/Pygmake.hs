import Control.Exception (catch, SomeException)
import Data.List
import System.Environment
import System.Exit
import System.Directory
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

  useCMake <- doesFileExist "CMakeLists.txt"
  if useCMake then executeCMake cf args
              else executeMake cf args

usage :: IO a
usage = do
  putStrLn $ "Usage: " ++ makeExecutable ++ " [make arguments]"
  putStrLn "CMake will be automatically invoked if CMakeLists.txt is present."
  exitWith (ExitFailure (-1))

parseArgs :: [String] -> IO [String]
parseArgs ["--help"] = usage
parseArgs ["-h"]     = usage
parseArgs as         = return as

executeMake:: Config -> [String] -> IO ()
executeMake cf args = do
    (_, _, _, handle) <- createProcess $ proc (make cf) newArgs
    ensureSuccess =<< waitForProcess handle
  where
    newArgs = [compilerEnvVar "CC" cc ccArgs,
               compilerEnvVar "CXX" cpp cppArgs]
              ++ (makeArgs cf) ++ args
    compilerEnvVar var cmd cmdArgs = intercalate " " $
      [(var ++ "=" ++ scanExecutable),
       "--make", (show . ifPort $ cf), cmd cf]
       ++ (cmdArgs cf)

executeCMake :: Config -> [String] -> IO ()
executeCMake cf args = do
    (_, _, _, handle) <- createProcess $ proc (cmake cf) newArgs
    ensureSuccess =<< waitForProcess handle
    -- Now a Makefile has been generated; should just be able to run make.
    (_, _, _, makeHandle) <- createProcess $ proc (make cf) []
    ensureSuccess =<< waitForProcess makeHandle
  where
    newArgs = ["-DCMAKE_C_COMPILER=" ++ scanExecutable,
               "-DCMAKE_C_FLAGS=" ++ (intercalate " " (["--make", p, cc cf]
                                                       ++ (ccArgs cf))),
               "-DCMAKE_CXX_COMPILER=" ++ scanExecutable,
               "-DCMAKE_CXX_FLAGS=" ++ (intercalate " " (["--make", p, cpp cf]
                                                         ++ (cppArgs cf)))]
              ++ (cmakeArgs cf) ++ args
    p = show . ifPort $ cf

handleRPCFailure :: SomeException -> IO ()
handleRPCFailure _ = putStrLn "Can't connect to pygd. Build information will not be recorded."

ensureSuccess :: ExitCode -> IO ()
ensureSuccess code@(ExitFailure _) = exitWith code
ensureSuccess _                    = return ()
