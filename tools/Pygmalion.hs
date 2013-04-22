{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.Maybe
import qualified Data.Text as T
import Safe (readMay)
import System.Environment
import System.Exit

import Pygmalion.Analysis.Source
import Pygmalion.Config
import Pygmalion.Core
import Pygmalion.RPC.Client
--import Pygmalion.JSON

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
  putStrLn   " --callers [file] [line] [col]"
  putStrLn   " --display-ast [file]"
  bail

parseArgs :: [String] -> IO ()
parseArgs ["--compile-commands"] = printCDB
parseArgs ["--flags-for-file", f] = printFlags (mkSourceFile f)
parseArgs ["--directory-for-file", f] = printDir (mkSourceFile f)
parseArgs ["--definition-for", f, line, col] = printDef (mkSourceFile f)
                                                        (readMay line) (readMay col)
parseArgs ["--callers", f, line, col] = printCallers (mkSourceFile f)
                                                     (readMay line) (readMay col)
parseArgs ["--display-ast", f] = printAST (mkSourceFile f)
parseArgs ["--help"] = usage
parseArgs ["-h"]     = usage
parseArgs _          = usage

-- FIXME: Reimplement with RPC.
printCDB :: IO ()
{-
printCDB = withDB $ \h -> getAllSourceFiles h >>= putStrLn . sourceRecordsToJSON
-}
printCDB = undefined

printFlags :: SourceFile -> IO ()
printFlags f = getConfiguration >>= getCommandInfoOr bail f >>= putFlags
  where putFlags (CommandInfo _ _ (Command _ args) _) = putStrLn . T.unpack . T.intercalate " " $ args

printDir :: SourceFile -> IO ()
printDir f = getConfiguration >>= getCommandInfoOr bail f >>= putDir
  where putDir (CommandInfo _ wd _ _) = putStrLn . T.unpack $ wd

getCommandInfoOr :: IO () -> SourceFile -> Config -> IO CommandInfo
getCommandInfoOr a f cf = do
  cmd <- rpcGetSimilarCommandInfo (ifPort cf) f
  unless (isJust cmd) a
  return . fromJust $ cmd

printDef :: SourceFile -> Maybe Int -> Maybe Int -> IO ()
printDef f (Just line) (Just col) = do
    cf <- getConfiguration
    cmd <- getCommandInfoOr (bailWith cmdErr) f cf
    info <- getLookupInfo cmd (SourceLocation f line col)
    case info of
      GotDef di  -> putDef di
      -- FIXME Clean this up
      GotDecl usr di -> do def <- rpcGetDefinition (ifPort cf) usr
                           if isJust def then putDef (fromJust def)
                                         else putDecl di
      GotUSR usr -> do def <- rpcGetDefinition (ifPort cf) usr
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
    putDecl (DefInfo n _ (SourceLocation idF idLine idCol) k) =
      putStrLn $ (unSourceFile idF) ++ ":" ++ (show idLine) ++ ":" ++ (show idCol) ++
                 ": Declaration: " ++ (T.unpack n) ++ " [" ++ (T.unpack k) ++ "]"
printDef _ _ _ = usage

printCallers :: SourceFile -> Maybe Int -> Maybe Int -> IO ()
printCallers f (Just line) (Just col) = do
    cf <- getConfiguration
    cmd <- getCommandInfoOr (bailWith cmdErr) f cf
    info <- getLookupInfo cmd (SourceLocation f line col)
    case info of
      GotDef di     -> printCallers' (diUSR di) cf
      GotDecl usr _ -> printCallers' usr cf
      GotUSR usr    -> printCallers' usr cf
      GotNothing    -> bailWith idErr
  where 
    errPrefix = (unSourceFile f) ++ ":" ++ (show line) ++ ":" ++ (show col) ++ ": "
    cmdErr = errPrefix ++ "No compilation information for this file."
    idErr = errPrefix ++ "No identifier at this location."
    defErr usr = errPrefix ++ "No callers for this identifier. USR = [" ++ (T.unpack usr) ++ "]"
    printCallers' usr cf = do
      callers <- rpcGetCallers (ifPort cf) usr
      case (null callers) of
        True  -> bailWith (defErr usr)
        False -> mapM_ putCaller callers
    putCaller (Invocation (DefInfo n _ _ _) (SourceLocation idF idLine idCol)) =
      putStrLn $ (unSourceFile idF) ++ ":" ++ (show idLine) ++ ":" ++ (show idCol) ++
                 ": Caller: " ++ (T.unpack n)
printCallers _ _ _ = usage


printAST :: SourceFile -> IO ()
printAST f = getConfiguration >>= getCommandInfoOr bail f >>= displayAST

bail :: IO ()
bail = exitWith (ExitFailure (-1))

bailWith :: String -> IO ()
bailWith s = putStrLn s >> bail
