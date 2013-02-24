import Control.Monad
import Database.SQLite
import System.Environment
import System.Exit
import Text.JSON

import Metadata
import SourceDB

main = getArgs
   >>= parseArgs
   >>  dumpJSON

queryExecutable = "pygmalion"

usage = putStrLn $ "Usage: " ++ queryExecutable ++ " --json"

parseArgs :: [String] -> IO ()
parseArgs ["--json"] = return ()
parseArgs ["--help"] = usage >> exitSuccess
parseArgs ["-h"]     = usage >> exitSuccess
parseArgs _          = usage >> exitSuccess

instance JSON Value where
  showJSON (Double d) = showJSON d
  showJSON (Int i)    = showJSON i
  showJSON (Text t)   = showJSON t
  showJSON (Blob b)   = showJSON b
  showJSON (Null)     = JSNull

  -- Don't need to read.
  readJSON _ = Error "Not implemented"

dumpJSON :: IO ()
dumpJSON = withDB $ \handle -> do
  records <- getAllRecords handle
  putStrLn . encodeStrict $ map toJSObject records
  -- forM_ records $ \r -> do
    -- putStrLn . encodeStrict $ toJSObject r
