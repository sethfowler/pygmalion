import Control.Monad
import System.Environment
import System.Exit
import System.Process

import Metadata
import SourceDB

main = getArgs
   >>= parseArgs
   >>= runMake

makeExecutable = "pygmake"

usage = putStrLn $ "Usage: " ++ makeExecutable ++ " [make arguments]"

parseArgs :: [String] -> IO [String]
parseArgs ["--help"] = usage >> exitSuccess
parseArgs ["-h"]     = usage >> exitSuccess
parseArgs as         = return as

runMake :: [String] -> IO ()
runMake as = do
  (_, _, _, handle) <- createProcess (proc "make" as)
  exitWith =<< waitForProcess handle
