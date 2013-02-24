import Control.Monad
import System.Environment
import System.Exit
import System.Process

import Pygmalion.JSON
import Pygmalion.SourceDB

main = getArgs
   >>= parseArgs
   >>= runMake
   >>= ensureSuccess
   >>  writeCompileCommands

makeExecutable = "pygmake"

usage = putStrLn $ "Usage: " ++ makeExecutable ++ " [make arguments]"

parseArgs :: [String] -> IO [String]
parseArgs ["--help"] = usage >> exitSuccess
parseArgs ["-h"]     = usage >> exitSuccess
parseArgs as         = return as

runMake :: [String] -> IO ExitCode
runMake as = do
    (_, _, _, handle) <- createProcess (proc "make" newArgs)
    waitForProcess handle
  where
    newArgs = ["CC=pygscan clang", "CXX=pygscan clang++"] ++ as

ensureSuccess :: ExitCode -> IO ()
ensureSuccess code@(ExitFailure _) = exitWith code
ensureSuccess _ = return ()

compileCommandsFile = "compile_commands.json"

writeCompileCommands :: IO ()
writeCompileCommands = withDB $ \h -> do
  getAllRecords h >>= (writeFile compileCommandsFile) . sourceRecordsToJSON
