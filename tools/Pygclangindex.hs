import Control.Applicative
import Control.Exception
import System.Environment
import System.Posix.Process

import Pygmalion.Core
import Pygmalion.Index.Source
import Pygmalion.Log
import Pygmalion.RPC.Client

main :: IO ()
main = do
    result <- try $ do
      initLogger INFO
      nice 10
      (port, ci) <- parseArgs =<< getArgs
      withRPCRaw port (runSourceAnalyses ci)
    case result of
      Left e   -> logError $ "Indexing process threw exception " ++ (show (e :: SomeException))
      Right () -> return ()

parseArgs :: [String] -> IO (Port, CommandInfo)
parseArgs (port : ci : []) = return (read port, read ci)
parseArgs _                = error "Invalid arguments"
