import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Maybe
import System.Directory
import System.Environment
import System.Path
import System.Posix.Process

import Pygmalion.Config
import Pygmalion.Core
import Pygmalion.Index.Source
import Pygmalion.Log
import Pygmalion.RPC.Client

main :: IO ()
main = do
  args <- getArgs
  result <- try $ do
    initLogger INFO
    nice 10
    parseArgs args
  case result of
    Left e   -> logError $ "While indexing with arguments "
                        ++ (show args)
                        ++ " process threw exception "
                        ++ (show (e :: SomeException))
    Right () -> return ()

parseArgs :: [String] -> IO ()
parseArgs ["--ast", file] = printAST file
parseArgs [path, ci]      = index path (read ci)
parseArgs _               = error "Invalid arguments"

index :: FilePath -> CommandInfo -> IO ()
index path ci = withRPCRaw path (runSourceAnalyses ci)
                
printAST :: FilePath -> IO ()
printAST f = do
    cf <- getConfiguration
    wd <- getCurrentDirectory
    let sf = asSourceFile wd f
    getCommandInfoOr (error err) sf cf >>= displayAST
  where err = "No compilation information for this file."
  
asSourceFile :: FilePath -> FilePath -> SourceFile
asSourceFile wd p = mkSourceFile $ maybe p id (absNormPath wd p)
                    
getCommandInfoOr :: IO () -> SourceFile -> Config -> IO CommandInfo
getCommandInfoOr a f cf = do
  cmd <- withRPC cf $ runRPC (rpcGetSimilarCommandInfo f)
  unless (isJust cmd) a
  return . fromJust $ cmd
