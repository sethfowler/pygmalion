import Control.Monad
import System.Environment
import System.Exit

import Pygmalion.Core
import Pygmalion.Database
import Pygmalion.JSON

main :: IO ()
main = getArgs
   >>= parseArgs
   >>  dumpJSON

usage :: IO ()
usage = putStrLn $ "Usage: " ++ queryExecutable ++ " --compile-commands"

parseArgs :: [String] -> IO ()
parseArgs ["--compile-commands"] = return ()
parseArgs ["--help"] = usage >> exitSuccess
parseArgs ["-h"]     = usage >> exitSuccess
parseArgs _          = usage >> exitSuccess

dumpJSON :: IO ()
dumpJSON = withDB dbFile $ \h ->
  getAllRecords h >>= putStrLn . sourceRecordsToJSON
