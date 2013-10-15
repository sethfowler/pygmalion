{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

import Control.Concurrent.Async
import Control.Exception (catch, SomeException)
import Control.Monad
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.UTF8 as B
import Data.List (intercalate)
import Data.Maybe
import Safe (readMay)
import System.Directory
import System.Environment
import System.Exit
import System.IO (withFile, IOMode(ReadWriteMode))
import System.Path
import System.Process

import Pygmalion.Config
import Pygmalion.Core
import Pygmalion.Index.Command
import Pygmalion.Log
import Pygmalion.RPC.Client
--import Pygmalion.JSON

main :: IO ()
main = do
  args <- getArgs
  parseConfiglessArgs args $ do
    cf <- getConfiguration
    initLogger (logLevel cf)
    wd <- getCurrentDirectory
    parseArgs cf wd args

usage :: IO ()
usage = do
  putStrLn $ "Usage: " ++ queryExecutable ++ " [command]"
  putStrLn   "where [command] is one of the following:"
  putStrLn   " help                      Prints this message."
  putStrLn   " start                     Starts the pygmalion daemon."
  putStrLn   " stop                      Terminates the pygmalion daemon."
  putStrLn   " init                      Initializes a pygmalion index rooted at"
  putStrLn   "                           the current directory."
  putStrLn   " index ([file]|[command])  Manually request indexing. If a single"
  putStrLn   "                           argument is provided, it's interpreted"
  putStrLn   "                           as a file to index using the default"
  putStrLn   "                           compiler flags. Multiple arguments are"
  putStrLn   "                           interpreted as a compiler invocation"
  putStrLn   "                           (e.g. 'clang -c file.c') and the compiler"
  putStrLn   "                           flags to use will be extracted appropriately."
  putStrLn   " make [command]            Both requests indexing and executes the"
  putStrLn   "                           provided command."
  putStrLn   " wait                      Blocks until all previous requests have been"
  putStrLn   "                           handled by the pygmalion daemon."
  putStrLn   " generate-compile-commands Prints a clang compilation database."
  putStrLn   " compile-flags [file]      Prints the compilation flags for the"
  putStrLn   "                           given file, or nothing on failure. If"
  putStrLn   "                           the file isn't in the database, a guess"
  putStrLn   "                           will be printed if possible."
  putStrLn   " working-directory [file]  Prints the working directory at the time"
  putStrLn   "                           the file was compiled. Guesses if needed."
  putStrLn   " definition [file] [line] [col]"
  putStrLn   " callers [file] [line] [col]"
  putStrLn   " callees [file] [line] [col]"
  putStrLn   " bases [file] [line] [col]"
  putStrLn   " overrides [file] [line] [col]"
  putStrLn   " members [file] [line] [col]"
  putStrLn   " references [file] [line] [col]"
  putStrLn   " hierarchy [file] [line] [col] Prints a graphviz graph showing the inheritance"
  putStrLn   "                               hierarchy of the identifier at this location."
  bail

parseConfiglessArgs :: [String] -> IO () -> IO ()
parseConfiglessArgs ["init"] _ = initialize
parseConfiglessArgs ["help"] _ = usage
parseConfiglessArgs []         _ = usage
parseConfiglessArgs _          c = c

parseArgs :: Config -> FilePath -> [String] -> IO ()
parseArgs _ _  ["start"] = start
parseArgs c _  ["stop"] = stop c
parseArgs c wd ("index" : file : []) = indexFile c (asSourceFile wd file)
parseArgs c _  ("index" : cmd : args) = indexCommand c cmd args
parseArgs c _  ("make" : cmd : args) = makeCommand c cmd args
parseArgs c _  ("wait" : []) = waitForServer c
parseArgs c _  ["generate-compile-commands"] = printCDB c
parseArgs c wd ["compile-flags", f] = printFlags c (asSourceFile wd f)
parseArgs c wd ["working-directory", f] = printDir c (asSourceFile wd f)
parseArgs c wd ["definition", f, line, col] = printDef c (asSourceFile wd f)
                                                         (readMay line) (readMay col)
parseArgs c wd ["callers", f, line, col] = printCallers c (asSourceFile wd f)
                                                          (readMay line) (readMay col)
parseArgs c wd ["callees", f, line, col] = printCallees c (asSourceFile wd f)
                                                          (readMay line) (readMay col)
parseArgs c wd ["bases", f, line, col] = printBases c (asSourceFile wd f)
                                                      (readMay line) (readMay col)
parseArgs c wd ["overrides", f, line, col] = printOverrides c (asSourceFile wd f)
                                                              (readMay line) (readMay col)
parseArgs c wd ["members", f, line, col] = printMembers c (asSourceFile wd f)
                                                          (readMay line) (readMay col)
parseArgs c wd ["references", f, line, col] = printRefs c (asSourceFile wd f)
                                                          (readMay line) (readMay col)
parseArgs c wd ["hierarchy", f, line, col] = printHierarchy c (asSourceFile wd f)
                                                              (readMay line) (readMay col)
parseArgs _ _ _           = usage

asSourceFile :: FilePath -> FilePath -> SourceFile
asSourceFile wd p = mkSourceFile $ maybe p id (absNormPath wd p)

start :: IO ()
start = void $ waitForProcess =<< runCommand "pygd"

stop :: Config -> IO ()
stop cf = withRPC cf $ runRPC rpcStop

initialize :: IO ()
initialize = do
  putStrLn "Initializing a pygmalion index..."
  createDirectoryIfMissing True pygmalionDir
  withFile configFile ReadWriteMode (\_ -> return ())

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

waitForServer :: Config -> IO ()
waitForServer cf = withRPC cf $ runRPC (rpcWait)

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
printDef = queryCmd "definition" rpcGetDefinition putDef
  where 
    putDef (DefInfo n _ (SourceLocation idF idLine idCol) k _) =
      putStrLn $ (unSourceFile idF) ++ ":" ++ (show idLine) ++ ":" ++ (show idCol) ++
                 ": Definition: " ++ (B.toString n) ++ " [" ++ (show k) ++ "]"

printCallers :: Config -> SourceFile -> Maybe Int -> Maybe Int -> IO ()
printCallers = queryCmd "caller" rpcGetCallers putCaller
  where 
    putCaller (Invocation (DefInfo n _ _ _ _) (SourceLocation idF idLine idCol)) =
      putStrLn $ (unSourceFile idF) ++ ":" ++ (show idLine) ++ ":" ++ (show idCol) ++
                 ": Caller: " ++ (B.toString n)

printCallees :: Config -> SourceFile -> Maybe Int -> Maybe Int -> IO ()
printCallees = queryCmd "callee" rpcGetCallees putCallee
  where 
    putCallee (DefInfo n _ (SourceLocation idF idLine idCol) k _) =
      putStrLn $ (unSourceFile idF) ++ ":" ++ (show idLine) ++ ":" ++ (show idCol) ++
                 ": Callee: " ++ (B.toString n) ++ " [" ++ (show k) ++ "]"

printBases :: Config -> SourceFile -> Maybe Int -> Maybe Int -> IO ()
printBases = queryCmd "base" rpcGetBases putBase
  where 
    putBase (DefInfo n _ (SourceLocation idF idLine idCol) k _) =
      putStrLn $ (unSourceFile idF) ++ ":" ++ (show idLine) ++ ":" ++ (show idCol) ++
                 ": Base: " ++ (B.toString n) ++ " [" ++ (show k) ++ "]"

printOverrides :: Config -> SourceFile -> Maybe Int -> Maybe Int -> IO ()
printOverrides = queryCmd "override" rpcGetOverrides putOverride
  where 
    putOverride (DefInfo n _ (SourceLocation idF idLine idCol) k _) =
      putStrLn $ (unSourceFile idF) ++ ":" ++ (show idLine) ++ ":" ++ (show idCol) ++
                 ": Override: " ++ (B.toString n) ++ " [" ++ (show k) ++ "]"

printMembers :: Config -> SourceFile -> Maybe Int -> Maybe Int -> IO ()
printMembers = queryCmd "member" rpcGetMembers putMember
  where 
    putMember (DefInfo n _ (SourceLocation idF idLine idCol) k _) =
      putStrLn $ (unSourceFile idF) ++ ":" ++ (show idLine) ++ ":" ++ (show idCol) ++
                 ": Member: " ++ (B.toString n) ++ " [" ++ (show k) ++ "]"

printRefs :: Config -> SourceFile -> Maybe Int -> Maybe Int -> IO ()
printRefs = queryCmd "reference" rpcGetRefs putRef
  where 
    putRef (SourceReference (SourceLocation idF idLine idCol) k ctx) =
      putStrLn $ (unSourceFile idF) ++ ":" ++ (show idLine) ++ ":" ++ (show idCol) ++
                 ": Reference: " ++ (B.toString ctx) ++ " [" ++ (show k) ++ "]"

printHierarchy :: Config -> SourceFile -> Maybe Int -> Maybe Int -> IO ()
printHierarchy cf file (Just line) (Just col) = do
  dotGraph <- withRPC cf $ runRPC (rpcGetHierarchy (SourceLocation file line col))
  case dotGraph of
    [] -> putStrLn "diGraph G {}"  -- Print an empty graph instead of an error.
    _  -> putStrLn dotGraph
printHierarchy _ _ _ _ = usage

queryCmd :: String -> (SourceLocation -> RPC [a]) -> (a -> IO ())
         -> Config -> SourceFile -> Maybe Int -> Maybe Int -> IO ()
queryCmd item rpcCall f cf file (Just line) (Just col) = do
    results <- withRPC cf $ runRPC (rpcCall (SourceLocation file line col))
    case results of
      [] -> bailWith (errMsg item file line col)
      _  -> mapM_ f results
queryCmd _ _ _ _ _ _ _ = usage

errMsg :: String -> SourceFile -> SourceLine -> SourceCol -> String
errMsg item f line col = errPrefix ++ "No " ++ item ++ " available for an identifier at this "
                                   ++ "location. Make sure the file compiles with no errors "
                                   ++ "and the index is up-to-date."
  where
    errPrefix = (unSourceFile f) ++ ":" ++ (show line) ++ ":" ++ (show col) ++ ": "

bail :: IO ()
bail = exitWith (ExitFailure (-1))

bail' :: ExitCode -> IO ()
bail' code = exitWith code

bailWith :: String -> IO ()
bailWith s = putStrLn s >> bail

bailWith' :: ExitCode -> String -> IO ()
bailWith' code s = putStrLn s >> bail' code
