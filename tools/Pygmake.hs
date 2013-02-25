import Control.Monad
import System.Directory
import System.Environment
import System.Exit
import System.FilePath.Posix
import System.Process

import Pygmalion.JSON
import Pygmalion.SourceDB

main = getArgs
   >>= parseArgs
   >>= runMake
   >>= ensureSuccess
   >>  writeCompileCommands

-- TODO: Put these somewhere more central.
scanExecutable = "pygscan"
makeExecutable = "pygmake"
clangExecutable = "clang"
clangppExecutable = "clang++"

usage = putStrLn $ "Usage: " ++ makeExecutable ++ " [make arguments]"

parseArgs :: [String] -> IO [String]
parseArgs ["--help"] = usage >> exitSuccess
parseArgs ["-h"]     = usage >> exitSuccess
parseArgs as         = return as

runMake :: [String] -> IO ExitCode
runMake as = do
    -- Ensure that the database exists.
    ensureDB dbFilename
    wd <- getCurrentDirectory
    let dbPath = combine wd dbFilename
    -- Run make.
    (_, _, _, handle) <- createProcess (proc "make" (newArgs dbPath))
    waitForProcess handle
  where
    newArgs d = [cc d, cxx d] ++ as
    cc d = "CC=" ++ scanExecutable ++ " " ++ d ++ " " ++ clangExecutable
    cxx d = "CXX=" ++ scanExecutable ++ " " ++ d ++ " " ++ clangppExecutable

ensureSuccess :: ExitCode -> IO ()
ensureSuccess code@(ExitFailure _) = exitWith code
ensureSuccess _                    = return ()

compileCommandsFile = "compile_commands.json"

writeCompileCommands :: IO ()
writeCompileCommands = withDB dbFilename $ \h -> do
  getAllRecords h >>= (writeFile compileCommandsFile) . sourceRecordsToJSON
