import Control.Monad
import Data.List
import System.Environment
import System.Exit

import Pygmalion.Core
import Pygmalion.Database
import Pygmalion.JSON

main :: IO ()
main = getArgs
   >>= parseArgs

usage :: IO ()
usage = do
  putStrLn $ "Usage: " ++ queryExecutable ++ " [command]"
  putStrLn   "where [command] is one of the following:"
  putStrLn   " --help                  Prints this message."
  putStrLn   " --compile-commands      Prints a clang compilation database."
  putStrLn   " --flags-for-file [file] Prints the compilation flags for the"
  putStrLn   "                         given file, or nothing on failure. If"
  putStrLn   "                         the file isn't in the database, a guess"
  putStrLn   "                         will be printed if possible."
  exitWith (ExitFailure (-1))

parseArgs :: [String] -> IO ()
parseArgs ["--compile-commands"] = printCDB
parseArgs ["--flags-for-file", path] = printFlags path
parseArgs ["--help"] = usage
parseArgs ["-h"]     = usage
parseArgs _          = usage

printCDB :: IO ()
printCDB = withDB dbFile $ \h ->
  getAllSourceFiles h >>= putStrLn . sourceRecordsToJSON

printFlags :: FilePath -> IO ()
printFlags f = withDB dbFile $ \h -> do
  preciseCmd <- getSourceFile h f
  case preciseCmd of
    Just (CommandInfo _ _ cmd _) -> putFlags cmd
    _                            -> printSimilarFlags h f

printSimilarFlags :: DBHandle -> FilePath -> IO ()
printSimilarFlags h f = do
  similarCmd <- getSimilarSourceFile h f
  case similarCmd of
    Just (CommandInfo _ _ cmd _) -> putFlags cmd
    _                            -> exitWith (ExitFailure (-1))

putFlags :: Command -> IO ()
putFlags (Command _ args) = putStrLn . intercalate " " $ args
