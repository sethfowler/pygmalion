{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad
import Data.Maybe
import qualified Data.Text as T
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
  bail

parseArgs :: [String] -> IO ()
parseArgs ["--compile-commands"] = printCDB
parseArgs ["--flags-for-file", f] = printFlags (mkSourceFile f)
parseArgs ["--directory-for-file", f] = printDir (mkSourceFile f)
parseArgs ["--definition-for", f, line, col] = printDef (mkSourceFile f)
                                                        (readMay line) (readMay col)
parseArgs ["--help"] = usage
parseArgs ["-h"]     = usage
parseArgs _          = usage

printCDB :: IO ()
printCDB = withDB $ \h -> getAllSourceFiles h >>= putStrLn . sourceRecordsToJSON

printFlags :: SourceFile -> IO ()
printFlags f = withDB (getCommandInfoOr bail f) >>= putFlags
  where putFlags (CommandInfo _ _ (Command _ args) _) = putStrLn . T.unpack . T.intercalate " " $ args

printDir :: SourceFile -> IO ()
printDir f = withDB (getCommandInfoOr bail f) >>= putDir
  where putDir (CommandInfo _ wd _ _) = putStrLn . T.unpack $ wd

getCommandInfoOr :: IO () -> SourceFile -> DBHandle -> IO CommandInfo
getCommandInfoOr a f h = do
  cmd <- liftM2 (<|>) (getCommandInfo h f) (getSimilarCommandInfo h f)
  unless (isJust cmd) a
  return . fromJust $ cmd

printDef :: SourceFile -> Maybe Int -> Maybe Int -> IO ()
printDef f (Just line) (Just col) = withDB $ \h -> do
    cmd <- getCommandInfoOr (bailWith cmdErr) f h
    info <- getLookupInfo cmd (SourceLocation f line col)
    case info of
      GotDef di  -> putDef di
      GotUSR usr -> do def <- getDef h usr
                       unless (isJust def) $ bailWith (defErr usr)
                       putDef (fromJust def)
      GotNothing -> bailWith idErr
  where 
    errPrefix = (unSourceFile f) ++ ":" ++ (show line) ++ ":" ++ (show col) ++ ": "
    cmdErr = errPrefix ++ "No compilation information for this file."
    idErr = errPrefix ++ "No identifier at this location."
    defErr usr = errPrefix ++ "No definition for this identifier. USR = [" ++ (T.unpack usr) ++ "]"
    putDef (DefInfo n _ (SourceLocation idF idLine idCol) k) =
      putStrLn $ (unSourceFile idF) ++ ":" ++ (show idLine) ++ ":" ++ (show idCol) ++
                 ": Definition: " ++ (T.unpack n) ++ " [" ++ (T.unpack k) ++ "]"
printDef _ _ _ = usage

bail :: IO ()
bail = exitWith (ExitFailure (-1))

bailWith :: String -> IO ()
bailWith s = putStrLn s >> bail
