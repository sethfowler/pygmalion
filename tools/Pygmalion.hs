import Control.Monad
import System.Environment
import System.Exit

import Pygmalion.JSON
import Pygmalion.Metadata
import Pygmalion.SourceDB

main = getArgs
   >>= parseArgs
   >>  dumpJSON

queryExecutable = "pygmalion"

usage = putStrLn $ "Usage: " ++ queryExecutable ++ " --compile-commands"

parseArgs :: [String] -> IO ()
parseArgs ["--compile-commands"] = return ()
parseArgs ["--help"] = usage >> exitSuccess
parseArgs ["-h"]     = usage >> exitSuccess
parseArgs _          = usage >> exitSuccess

dumpJSON :: IO ()
dumpJSON = withDB $ \h -> getAllRecords h >>= putStrLn . sourceRecordsToJSON
