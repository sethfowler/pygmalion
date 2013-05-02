import Control.Applicative
import Control.Exception
import qualified Data.ByteString.UTF8 as B
import System.Directory
import System.Environment
import System.Posix.Process

import Pygmalion.Analysis.Source
import Pygmalion.Core
import Pygmalion.Log
import Pygmalion.RPC.Client

main :: IO ()
main = do
    result <- try $ do
      initLogger INFO
      nice 10
      (port, ci) <- parseArgs =<< getArgs
      wd <- B.fromString <$> getCurrentDirectory
      withRPCRaw port (runSourceAnalyses wd ci)
    case result of
      Left e   -> logError $ "Indexing process threw exception " ++ (show (e :: SomeException))
      Right () -> return ()

parseArgs :: [String] -> IO (Port, CommandInfo)
parseArgs (port : ci : []) = return (read port, read ci)
parseArgs _                = error "Invalid arguments"
