import Control.Applicative
import Data.List
import System.Environment
import System.Exit
import System.Process

import Pygmalion.Core
import Pygmalion.Config
--import Pygmalion.JSON

main :: IO ()
main = do
  args <- (getArgs >>= parseArgs)
  cf <- getConfiguration
  ensureSuccess =<< executeMake cf (ifPort cf) args
  --when (makeCDB cf) writeCompileCommands

usage :: IO a
usage = putStrLn ("Usage: " ++ makeExecutable ++ " [make arguments]")
     >> exitWith (ExitFailure (-1))

parseArgs :: [String] -> IO [String]
parseArgs ["--help"] = usage
parseArgs ["-h"]     = usage
parseArgs as         = return as

executeMake:: Config -> Port -> [String] -> IO ExitCode
executeMake cf p args = do
    let combinedArgs = newArgs . show $ p
    (_, _, _, handle) <- createProcess $ proc (make cf) combinedArgs
    waitForProcess handle
  where
    newArgs port = [compilerEnvVar "CC" port cc ccArgs,
                    compilerEnvVar "CXX" port cpp cppArgs]
                   ++ (makeArgs cf) ++ args
    compilerEnvVar var port cmd cmdArgs = intercalate " " $
      [(var ++ "=" ++ scanExecutable), "--make", port, cmd cf] ++ (cmdArgs cf)

ensureSuccess :: ExitCode -> IO ()
ensureSuccess code@(ExitFailure _) = exitWith code
ensureSuccess _                    = return ()

-- FIXME: Needs to be done over RPC now.
{-
writeCompileCommands :: IO ()
writeCompileCommands = withDB $ \h -> do
  getAllSourceFiles h >>= (writeFile compileCommandsFile) . sourceRecordsToJSON
-}
