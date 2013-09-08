{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

import Control.Concurrent.Async
import Control.Exception (catch, SomeException)
import Control.Monad
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.UTF8 as B
import Data.List
import Data.Maybe
import Data.Ord
import Safe (readMay)
import System.Directory
import System.Environment
import System.Exit
import System.Path
import System.Process

import Pygmalion.Config
import Pygmalion.Core
import Pygmalion.Index.Command
import Pygmalion.Index.Result
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
  putStrLn   " --start                     Starts the pygmalion daemon."
  putStrLn   " --stop                      Terminates the pygmalion daemon."
  putStrLn   " --index ([file]|[command])  Manually request indexing. If a single"
  putStrLn   "                             argument is provided, it's interpreted"
  putStrLn   "                             as a file to index using the default"
  putStrLn   "                             compiler flags. Multiple arguments are"
  putStrLn   "                             interpreted as a compiler invocation"
  putStrLn   "                             (e.g. 'clang -c file.c') and the compiler"
  putStrLn   "                             flags to use will be extracted appropriately."
  putStrLn   " --make [command]            Both requests indexing and executes the"
  putStrLn   "                             provided command."
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
  bail

parseArgs :: Config -> FilePath -> [String] -> IO ()
parseArgs _ _  ["--start"] = start
parseArgs c _  ["--stop"] = stop c
parseArgs c wd ("--index" : file : []) = indexFile c (asSourceFile wd file)
parseArgs c _  ("--index" : cmd : args) = indexCommand c cmd args
parseArgs c _  ("--make" : cmd : args) = makeCommand c cmd args
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
parseArgs _ _  ["--help"] = usage
parseArgs _ _  ["-h"]     = usage
parseArgs _ _ _           = usage

asSourceFile :: FilePath -> FilePath -> SourceFile
asSourceFile wd p = mkSourceFile $ maybe p id (absNormPath wd p)

start :: IO ()
start = void $ waitForProcess =<< runCommand "pygd"

stop :: Config -> IO ()
stop cf = withRPC cf $ runRPC rpcStop

indexCommand :: Config -> String -> [String] -> IO ()
indexCommand cf cmd args = do
  result <- getCommandInfo cmd args
  case result of
    Just ci -> withRPC cf $ runRPC (rpcIndexCommand ci)
    _       -> return ()   -- Can't do anything with this command.

indexFile :: Config -> SourceFile -> IO ()
indexFile cf f = withRPC cf $ runRPC (rpcIndexFile f)

makeCommand :: Config -> String -> [String] -> IO ()
makeCommand cf cmd args = do
    void $ concurrently invoke (indexCommand cf cmd args `catch` ignoreRPCFailure)
  where
    invoke = do
      (_, _, _, handle) <- createProcess (proc cmd args)
      code <- waitForProcess handle
      case code of
        ExitSuccess -> return ()
        _           -> liftIO $ bailWith' code $ queryExecutable ++ ": Command failed"

ignoreRPCFailure :: SomeException -> IO ()
ignoreRPCFailure _ = return ()  -- Silently ignore failure.

-- FIXME: Reimplement with RPC.
printCDB :: Config -> IO ()
{-
printCDB = withDB $ \h -> getAllSourceFiles h >>= putStrLn . sourceRecordsToJSON
-}
printCDB = undefined

printFlags :: Config -> SourceFile -> IO ()
printFlags cf f = getCommandInfoOr bail f cf >>= putFlags
  where
    putFlags (ciArgs -> args) = putStrLn . intercalate " " . map B.toString $ args

printDir :: Config -> SourceFile -> IO ()
printDir cf f = getCommandInfoOr bail f cf >>= putDir
  where putDir (ciWorkingPath -> wd) = putStrLn . B.toString $ wd

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
    defErr usr = errPrefix ++ "No definition for this identifier. USR = [" ++ (B.toString usr) ++ "]"
    putDef (DefInfo n _ (SourceLocation idF idLine idCol) k) =
      putStrLn $ (unSourceFile idF) ++ ":" ++ (show idLine) ++ ":" ++ (show idCol) ++
                 ": Definition: " ++ (B.toString n) ++ " [" ++ (show k) ++ "]"
    putDecl (DefInfo n _ (SourceLocation idF idLine idCol) k) =
      putStrLn $ (unSourceFile idF) ++ ":" ++ (show idLine) ++ ":" ++ (show idCol) ++
                 ": Declaration: " ++ (B.toString n) ++ " [" ++ (show k) ++ "]"
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
    defErr usr = errPrefix ++ "No callers for this identifier. USR = [" ++ (B.toString usr) ++ "]"
    printCallers' usr = do
      callers <- withRPC cf $ runRPC (rpcGetCallers usr)
      case (null callers) of
        True  -> bailWith (defErr usr)
        False -> mapM_ putCaller callers
    putCaller (Invocation (DefInfo n _ _ _) (SourceLocation idF idLine idCol)) =
      putStrLn $ (unSourceFile idF) ++ ":" ++ (show idLine) ++ ":" ++ (show idCol) ++
                 ": Caller: " ++ (B.toString n)
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
    defErr usr = errPrefix ++ "No callees for this identifier. USR = [" ++ (B.toString usr) ++ "]"
    printCallees' usr = do
      callees <- withRPC cf $ runRPC (rpcGetCallees usr)
      case (null callees) of
        True  -> bailWith (defErr usr)
        False -> mapM_ putCallee callees
    putCallee (DefInfo n _ (SourceLocation idF idLine idCol) k) =
      putStrLn $ (unSourceFile idF) ++ ":" ++ (show idLine) ++ ":" ++ (show idCol) ++
                 ": Callee: " ++ (B.toString n) ++ " [" ++ (show k) ++ "]"
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
    defErr usr = errPrefix ++ "No bases for this identifier. USR = [" ++ (B.toString usr) ++ "]"
    printBases' usr = do
      callers <- withRPC cf $ runRPC (rpcGetBases usr)
      case (null callers) of
        True  -> bailWith (defErr usr)
        False -> mapM_ putBase callers
    putBase (DefInfo n _ (SourceLocation idF idLine idCol) k) =
      putStrLn $ (unSourceFile idF) ++ ":" ++ (show idLine) ++ ":" ++ (show idCol) ++
                 ": Base: " ++ (B.toString n) ++ " [" ++ (show k) ++ "]"
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
    defErr usr = errPrefix ++ "No overrides for this identifier. USR = [" ++ (B.toString usr) ++ "]"
    printOverrides' usr = do
      callees <- withRPC cf $ runRPC (rpcGetOverrides usr)
      case (null callees) of
        True  -> bailWith (defErr usr)
        False -> mapM_ putOverride callees
    putOverride (DefInfo n _ (SourceLocation idF idLine idCol) k) =
      putStrLn $ (unSourceFile idF) ++ ":" ++ (show idLine) ++ ":" ++ (show idCol) ++
                 ": Override: " ++ (B.toString n) ++ " [" ++ (show k) ++ "]"
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
    defErr usr = errPrefix ++ "No references for this identifier. USR = [" ++ (B.toString usr) ++ "]"
    printRefs' usr = do
      refs <- withRPC cf $ runRPC (rpcGetRefs usr)
      case (null refs) of
        True  -> bailWith (defErr usr)
        False -> mapM_ putRef refs
    putRef (SourceReference (SourceLocation idF idLine idCol) k ctx) =
      putStrLn $ (unSourceFile idF) ++ ":" ++ (show idLine) ++ ":" ++ (show idCol) ++
                 ": Reference: " ++ (B.toString ctx) ++ " [" ++ (show k) ++ "]"
printRefs _ _ _ _ = usage

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

bail' :: ExitCode -> IO ()
bail' code = exitWith code

bailWith :: String -> IO ()
bailWith s = putStrLn s >> bail

bailWith' :: ExitCode -> String -> IO ()
bailWith' code s = putStrLn s >> bail' code
