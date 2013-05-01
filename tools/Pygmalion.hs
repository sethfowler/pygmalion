{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

import Control.Monad
import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Text as T
import Safe (readMay)
import System.Directory
import System.Environment
import System.Exit
import System.Path

import Pygmalion.Analysis.Source
import Pygmalion.Config
import Pygmalion.Core
import Pygmalion.Log
import Pygmalion.RPC.Client
--import Pygmalion.JSON

main :: IO ()
main = do
  cf <- getConfiguration
  initLogger (logLevel cf)
  args <- getArgs
  wd <- getCurrentDirectory
  parseArgs cf wd args

usage :: IO ()
usage = do
  putStrLn $ "Usage: " ++ queryExecutable ++ " [command]"
  putStrLn   "where [command] is one of the following:"
  putStrLn   " --help                      Prints this message."
  putStrLn   " --generate-compile-commands Prints a clang compilation database."
  putStrLn   " --compile-flags [file]      Prints the compilation flags for the"
  putStrLn   "                             given file, or nothing on failure. If"
  putStrLn   "                             the file isn't in the database, a guess"
  putStrLn   "                             will be printed if possible."
  putStrLn   " --working-directory [file]  Prints the working directory at the time"
  putStrLn   "                             the file was compiled. Guesses if needed."
  putStrLn   " --definition [file] [line] [col]"
  putStrLn   " --callers [file] [line] [col]"
  putStrLn   " --callees [file] [line] [col]"
  putStrLn   " --bases [file] [line] [col]"
  putStrLn   " --overrides [file] [line] [col]"
  putStrLn   " --references [file] [line] [col]"
  putStrLn   " --display-ast [file]"
  bail

parseArgs :: Config -> FilePath -> [String] -> IO ()
parseArgs c _  ["--generate-compile-commands"] = printCDB c
parseArgs c wd ["--compile-flags", f] = printFlags c (asSourceFile wd f)
parseArgs c wd ["--working-directory", f] = printDir c (asSourceFile wd f)
parseArgs c wd ["--definition", f, line, col] = printDef c (asSourceFile wd f)
                                                           (readMay line) (readMay col)
parseArgs c wd ["--callers", f, line, col] = printCallers c (asSourceFile wd f)
                                                            (readMay line) (readMay col)
parseArgs c wd ["--callees", f, line, col] = printCallees c (asSourceFile wd f)
                                                            (readMay line) (readMay col)
parseArgs c wd ["--bases", f, line, col] = printBases c (asSourceFile wd f)
                                                        (readMay line) (readMay col)
parseArgs c wd ["--overrides", f, line, col] = printOverrides c (asSourceFile wd f)
                                                                (readMay line) (readMay col)
parseArgs c wd ["--references", f, line, col] = printRefs c (asSourceFile wd f)
                                                            (readMay line) (readMay col)
parseArgs c wd ["--display-ast", f] = printAST c (asSourceFile wd f)
parseArgs _ _  ["--help"] = usage
parseArgs _ _  ["-h"]     = usage
parseArgs _ _ _           = usage

asSourceFile :: FilePath -> FilePath -> SourceFile
asSourceFile wd p = mkSourceFile $ maybe p id (absNormPath wd p)

-- FIXME: Reimplement with RPC.
printCDB :: Config -> IO ()
{-
printCDB = withDB $ \h -> getAllSourceFiles h >>= putStrLn . sourceRecordsToJSON
-}
printCDB = undefined

printFlags :: Config -> SourceFile -> IO ()
printFlags cf f = getCommandInfoOr bail f cf >>= putFlags
  where putFlags (ciArgs -> args) = putStrLn . T.unpack . T.intercalate " " $ args

printDir :: Config -> SourceFile -> IO ()
printDir cf f = getCommandInfoOr bail f cf >>= putDir
  where putDir (ciWorkingPath -> wd) = putStrLn . T.unpack $ wd

getCommandInfoOr :: IO () -> SourceFile -> Config -> IO CommandInfo
getCommandInfoOr a f cf = do
  cmd <- withRPC cf $ runRPC (rpcGetSimilarCommandInfo f)
  unless (isJust cmd) a
  return . fromJust $ cmd

printDef :: Config -> SourceFile -> Maybe Int -> Maybe Int -> IO ()
printDef cf f (Just line) (Just col) = do
    info <- doGetLookupInfo cf (SourceLocation f line col)
    case info of
      GotDef di  -> putDef di
      -- FIXME Clean this up
      GotDecl usr di -> do def <- withRPC cf $ runRPC (rpcGetDefinition usr)
                           if isJust def then putDef (fromJust def)
                                         else putDecl di
      GotUSR usr -> do def <- withRPC cf $ runRPC (rpcGetDefinition usr)
                       unless (isJust def) $ bailWith (defErr usr)
                       putDef (fromJust def)
      GotNothing -> bailWith idErr
  where 
    errPrefix = (unSourceFile f) ++ ":" ++ (show line) ++ ":" ++ (show col) ++ ": "
    idErr = errPrefix ++ "No identifier at this location."
    defErr usr = errPrefix ++ "No definition for this identifier. USR = [" ++ (T.unpack usr) ++ "]"
    putDef (DefInfo n _ (SourceLocation idF idLine idCol) k) =
      putStrLn $ (unSourceFile idF) ++ ":" ++ (show idLine) ++ ":" ++ (show idCol) ++
                 ": Definition: " ++ (T.unpack n) ++ " [" ++ (show k) ++ "]"
    putDecl (DefInfo n _ (SourceLocation idF idLine idCol) k) =
      putStrLn $ (unSourceFile idF) ++ ":" ++ (show idLine) ++ ":" ++ (show idCol) ++
                 ": Declaration: " ++ (T.unpack n) ++ " [" ++ (show k) ++ "]"
printDef _ _ _ _ = usage

printCallers :: Config -> SourceFile -> Maybe Int -> Maybe Int -> IO ()
printCallers cf f (Just line) (Just col) = do
    info <- doGetLookupInfo cf (SourceLocation f line col)
    case info of
      GotDef di     -> printCallers' (diUSR di)
      GotDecl usr _ -> printCallers' usr
      GotUSR usr    -> printCallers' usr
      GotNothing    -> bailWith idErr
  where 
    errPrefix = (unSourceFile f) ++ ":" ++ (show line) ++ ":" ++ (show col) ++ ": "
    idErr = errPrefix ++ "No identifier at this location."
    defErr usr = errPrefix ++ "No callers for this identifier. USR = [" ++ (T.unpack usr) ++ "]"
    printCallers' usr = do
      callers <- withRPC cf $ runRPC (rpcGetCallers usr)
      case (null callers) of
        True  -> bailWith (defErr usr)
        False -> mapM_ putCaller callers
    putCaller (Invocation (DefInfo n _ _ _) (SourceLocation idF idLine idCol)) =
      putStrLn $ (unSourceFile idF) ++ ":" ++ (show idLine) ++ ":" ++ (show idCol) ++
                 ": Caller: " ++ (T.unpack n)
printCallers _ _ _ _ = usage

printCallees :: Config -> SourceFile -> Maybe Int -> Maybe Int -> IO ()
printCallees cf f (Just line) (Just col) = do
    info <- doGetLookupInfo cf (SourceLocation f line col)
    case info of
      GotDef di     -> printCallees' (diUSR di)
      GotDecl usr _ -> printCallees' usr
      GotUSR usr    -> printCallees' usr
      GotNothing    -> bailWith idErr
  where 
    errPrefix = (unSourceFile f) ++ ":" ++ (show line) ++ ":" ++ (show col) ++ ": "
    idErr = errPrefix ++ "No identifier at this location."
    defErr usr = errPrefix ++ "No callees for this identifier. USR = [" ++ (T.unpack usr) ++ "]"
    printCallees' usr = do
      callees <- withRPC cf $ runRPC (rpcGetCallees usr)
      case (null callees) of
        True  -> bailWith (defErr usr)
        False -> mapM_ putCallee callees
    putCallee (DefInfo n _ (SourceLocation idF idLine idCol) k) =
      putStrLn $ (unSourceFile idF) ++ ":" ++ (show idLine) ++ ":" ++ (show idCol) ++
                 ": Callee: " ++ (T.unpack n) ++ " [" ++ (show k) ++ "]"
printCallees _ _ _ _ = usage

printBases :: Config -> SourceFile -> Maybe Int -> Maybe Int -> IO ()
printBases cf f (Just line) (Just col) = do
    info <- doGetLookupInfo cf (SourceLocation f line col)
    case info of
      GotDef di     -> printBases' (diUSR di)
      GotDecl usr _ -> printBases' usr
      GotUSR usr    -> printBases' usr
      GotNothing    -> bailWith idErr
  where 
    errPrefix = (unSourceFile f) ++ ":" ++ (show line) ++ ":" ++ (show col) ++ ": "
    idErr = errPrefix ++ "No identifier at this location."
    defErr usr = errPrefix ++ "No bases for this identifier. USR = [" ++ (T.unpack usr) ++ "]"
    printBases' usr = do
      callers <- withRPC cf $ runRPC (rpcGetBases usr)
      case (null callers) of
        True  -> bailWith (defErr usr)
        False -> mapM_ putBase callers
    putBase (DefInfo n _ (SourceLocation idF idLine idCol) k) =
      putStrLn $ (unSourceFile idF) ++ ":" ++ (show idLine) ++ ":" ++ (show idCol) ++
                 ": Base: " ++ (T.unpack n) ++ " [" ++ (show k) ++ "]"
printBases _ _ _ _ = usage

printOverrides :: Config -> SourceFile -> Maybe Int -> Maybe Int -> IO ()
printOverrides cf f (Just line) (Just col) = do
    info <- doGetLookupInfo cf (SourceLocation f line col)
    case info of
      GotDef di     -> printOverrides' (diUSR di)
      GotDecl usr _ -> printOverrides' usr
      GotUSR usr    -> printOverrides' usr
      GotNothing    -> bailWith idErr
  where 
    errPrefix = (unSourceFile f) ++ ":" ++ (show line) ++ ":" ++ (show col) ++ ": "
    idErr = errPrefix ++ "No identifier at this location."
    defErr usr = errPrefix ++ "No overrides for this identifier. USR = [" ++ (T.unpack usr) ++ "]"
    printOverrides' usr = do
      callees <- withRPC cf $ runRPC (rpcGetOverrides usr)
      case (null callees) of
        True  -> bailWith (defErr usr)
        False -> mapM_ putOverride callees
    putOverride (DefInfo n _ (SourceLocation idF idLine idCol) k) =
      putStrLn $ (unSourceFile idF) ++ ":" ++ (show idLine) ++ ":" ++ (show idCol) ++
                 ": Override: " ++ (T.unpack n) ++ " [" ++ (show k) ++ "]"
printOverrides _ _ _ _ = usage

printRefs :: Config -> SourceFile -> Maybe Int -> Maybe Int -> IO ()
printRefs cf f (Just line) (Just col) = do
    info <- doGetLookupInfo cf (SourceLocation f line col)
    case info of
      GotDef di     -> printRefs' (diUSR di)
      GotDecl usr _ -> printRefs' usr
      GotUSR usr    -> printRefs' usr
      GotNothing    -> bailWith idErr
  where 
    errPrefix = (unSourceFile f) ++ ":" ++ (show line) ++ ":" ++ (show col) ++ ": "
    idErr = errPrefix ++ "No identifier at this location."
    defErr usr = errPrefix ++ "No references for this identifier. USR = [" ++ (T.unpack usr) ++ "]"
    printRefs' usr = do
      refs <- withRPC cf $ runRPC (rpcGetRefs usr)
      case (null refs) of
        True  -> bailWith (defErr usr)
        False -> mapM_ putRef refs
    putRef (SourceReference (SourceLocation idF idLine idCol) k ctx) =
      putStrLn $ (unSourceFile idF) ++ ":" ++ (show idLine) ++ ":" ++ (show idCol) ++
                 ": Reference: " ++ (T.unpack ctx) ++ " [" ++ (show k) ++ "]"
printRefs _ _ _ _ = usage

printAST :: Config -> SourceFile -> IO ()
printAST cf f = getCommandInfoOr (bailWith err) f cf >>= displayAST
  where err = "No compilation information for this file."

{-
doGetLookupInfo :: Config -> SourceLocation -> IO LookupInfo
doGetLookupInfo cf sl = do
    cmd <- getCommandInfoOr (bailWith cmdErr) (slFile sl) cf
    getLookupInfo cmd sl
  where
    errPrefix = (unSourceFile $ slFile sl) ++
                ":" ++ (show $ slLine sl) ++ ":" ++ (show $ slCol sl) ++ ": "
    cmdErr = errPrefix ++ "No compilation information for this file."
-}

doGetLookupInfo :: Config -> SourceLocation -> IO LookupInfo
doGetLookupInfo cf sl = do
    rawReferenced <- withRPC cf $ runRPC (rpcGetReferenced sl)
    when (multipleItems rawReferenced) $ do
      logInfo "Warning: multiple referenced entities"
      logInfo (show sl)
      mapM_ (logInfo . show) rawReferenced
    let referenced = filterNarrowest . filterExpansion $ rawReferenced
    case referenced of
      Just referenced' -> return (GotDef . sdDef $ referenced')
      Nothing          -> return GotNothing
  where
    filterExpansion rs = let exps = filter ((== MacroExpansion) . sdKind) rs in
                         if null exps then rs else exps
    filterNarrowest [] = Nothing
    filterNarrowest rs = Just $ minimumBy (comparing $ rangeSize . sdRange) rs
    rangeSize (SourceRange _ l c el ec) = (el - l, ec - c)
    multipleItems [] = False
    multipleItems (_ : []) = False
    multipleItems _ = True

bail :: IO ()
bail = exitWith (ExitFailure (-1))

bailWith :: String -> IO ()
bailWith s = putStrLn s >> bail
