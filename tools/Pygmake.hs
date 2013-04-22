import Control.Applicative
import Data.List
import System.Environment
import System.Exit
import System.Process

import Pygmalion.Core
import Pygmalion.Config
import Pygmalion.RPC.Client
--import Pygmalion.JSON

main :: IO ()
main = do
  args <- (getArgs >>= parseArgs)
  cf <- getConfiguration
  sendPing (ifPort cf)  -- Make sure pygd is running.
  ensureSuccess =<< executeMake cf args
  --when (makeCDB cf) writeCompileCommands

usage :: IO a
usage = putStrLn ("Usage: " ++ makeExecutable ++ " [make arguments]")
     >> exitWith (ExitFailure (-1))

parseArgs :: [String] -> IO [String]
parseArgs ["--help"] = usage
parseArgs ["-h"]     = usage
parseArgs as         = return as

executeMake:: Config -> [String] -> IO ExitCode
executeMake cf args = do
    (_, _, _, handle) <- createProcess $ proc (make cf) newArgs
    waitForProcess handle
  where
    newArgs = [compilerEnvVar "CC" cc ccArgs,
               compilerEnvVar "CXX" cpp cppArgs]
              ++ (makeArgs cf) ++ args
    compilerEnvVar var cmd cmdArgs = intercalate " " $
      [(var ++ "=" ++ scanExecutable), "--make", show $ ifPort cf, cmd cf] ++ (cmdArgs cf)

ensureSuccess :: ExitCode -> IO ()
ensureSuccess code@(ExitFailure _) = exitWith code
ensureSuccess _                    = return ()

-- FIXME: Needs to be done over RPC now.
{-
writeCompileCommands :: IO ()
writeCompileCommands = withDB $ \h -> do
  getAllSourceFiles h >>= (writeFile compileCommandsFile) . sourceRecordsToJSON
-}
