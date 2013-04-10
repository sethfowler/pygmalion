import Control.Monad
import Data.List
import Safe (readMay)
import System.Environment
import System.Exit

import Pygmalion.Analyze.Source
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
  putStrLn   " --help                      Prints this message."
  putStrLn   " --compile-commands          Prints a clang compilation database."
  putStrLn   " --flags-for-file [file]     Prints the compilation flags for the"
  putStrLn   "                             given file, or nothing on failure. If"
  putStrLn   "                             the file isn't in the database, a guess"
  putStrLn   "                             will be printed if possible."
  putStrLn   " --directory-for-file [file] Prints the working directory at the time"
  putStrLn   "                             the file was compiled. Guesses if needed."
  putStrLn   " --definition-for [file] [line] [col]"
  exitWith (ExitFailure (-1))

parseArgs :: [String] -> IO ()
parseArgs ["--compile-commands"] = printCDB
parseArgs ["--flags-for-file", path] = printFlags path
parseArgs ["--directory-for-file", path] = printDir path
parseArgs ["--definition-for", file, line, col] = printDef file (readMay line) (readMay col)
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

-- FIXME: Ugh. Just hacking this in real quick; needs cleanup.
-- In fact printFlags needs cleanup too.
printDir :: FilePath -> IO ()
printDir f = withDB dbFile $ \h -> do
  preciseCmd <- getSourceFile h f
  case preciseCmd of
    Just (CommandInfo _ wd _ _) -> putStrLn wd
    _                           -> printSimilarDir h f

printSimilarDir :: DBHandle -> FilePath -> IO ()
printSimilarDir h f = do
  similarCmd <- getSimilarSourceFile h f
  case similarCmd of
    Just (CommandInfo _ wd _ _) -> putStrLn wd
    _                           -> exitWith (ExitFailure (-1))

-- FIXME: Ugh. Again hacked in real quick. Terrible code.
printDef :: FilePath -> Maybe Int -> Maybe Int -> IO ()
printDef f (Just line) (Just col) = withDB dbFile $ \h -> do
  cmd <- getSourceFile h f
  case cmd of
    Just ci -> printDef' h ci (SourceLocation f line col)
    Nothing -> do putStrLn $ f ++ ":" ++ (show line) ++ ":" ++ (show col) ++ ": No database entry for this file."
                  exitWith (ExitFailure (-1))
printDef _ _ _ = usage

printDef' :: DBHandle -> CommandInfo -> SourceLocation -> IO ()
printDef' h cmd sl@(SourceLocation f line col) = do
  ident <- getIdentifier cmd sl
  case ident of
    Just i -> printDef'' h sl i
    Nothing -> do putStrLn $ f ++ ":" ++ (show line) ++ ":" ++ (show col) ++ ": No identifier at this location."
                  exitWith (ExitFailure (-1))

printDef'' :: DBHandle -> SourceLocation -> Identifier -> IO ()
printDef'' h (SourceLocation f line col) i@(Identifier n _) = do
  loc <- getDef h i
  case loc of
    Just (DefInfo _ (SourceLocation idF idLine idCol) k) ->
      putStrLn $ idF ++ ":" ++ (show idLine) ++ ":" ++ (show idCol) ++
                 ": Definition: " ++ n ++ " [" ++ k ++ "]"
    Nothing -> do putStrLn $ f ++ ":" ++ (show line) ++ ":" ++ (show col) ++ ": No database entry for this identifier."
                  exitWith (ExitFailure (-1))
