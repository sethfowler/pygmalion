import Control.Monad
import Data.List
import Data.Time.Clock.POSIX
import System.Directory
import System.Environment
import System.Exit
import System.Process

import Pygmalion.Metadata
import Pygmalion.SourceDB

main = getArgs
   >>= parseArgs
   >>= runCmd
   >>= analyzeCmd
   >>= analyzeCode
   >>= updateDB

scanExecutable = "pygscan"

usage   = putStrLn $ "Usage: " ++ scanExecutable ++ " [command]"
die s c = putStrLn (scanExecutable ++ ": " ++ s) >> exitWith c

parseArgs :: [String] -> IO Command
parseArgs ["--help"] = usage >> exitSuccess
parseArgs ["-h"]     = usage >> exitSuccess
parseArgs []         = usage >> exitSuccess
parseArgs as         = return as

runCmd :: Command -> IO Command
runCmd cmd@(c : as) = do
  (_, _, _, handle) <- createProcess (proc c as)
  code <- waitForProcess handle
  case code of
    ExitSuccess -> return cmd
    _           -> die "Command failed" code

analyzeCmd :: Command -> IO CommandInfo
analyzeCmd cmd@(c : as) = do
    wd <- getCurrentDirectory
    time <- getPOSIXTime
    case sourceFile of
      Just sf -> return $ CommandInfo sf wd (c : filteredArgs) (floor time)
      _       -> die "Couldn't identify source filename" ExitSuccess
  where sourceFile   = find hasSourceExtension filteredArgs
        filteredArgs = filterArgs as

sourceExtensions = [".c", ".cc", ".cpp", ".C"]

hasSourceExtension :: String -> Bool
hasSourceExtension a = any (`isSuffixOf` a) sourceExtensions

-- We need to filter arguments that cause dependency files to be generated,
-- as they'll gum up the works when we use libclang to analyze files later.
filterArgs :: [String] -> [String]
filterArgs ("-MD" : as) = as
filterArgs ("-MF" : a : as) = as
filterArgs (a : as) | "-W" `isPrefixOf` a && "-MD" `isInfixOf` a = as
filterArgs (a : as) = a : filterArgs as
filterArgs [] = []

-- XXX: Just passthrough for now.
analyzeCode :: CommandInfo -> IO CommandInfo
analyzeCode = return

updateDB :: CommandInfo -> IO ()
updateDB cmdInfo = withDB $ \handle -> do
  updateRecord handle cmdInfo
