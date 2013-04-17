import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Conduit
import Data.Conduit.Binary
import Data.Conduit.Cereal
import Data.Maybe
import Data.Serialize
import qualified Data.Text as T
import System.Directory
import System.IO

import Pygmalion.Analysis.ClangRequest
import Pygmalion.Analysis.Source
import Pygmalion.Core

main :: IO ()
main = do
    wd <- T.pack <$> getCurrentDirectory
    sas <- mkSourceAnalysisState wd
    sourceHandle stdin $= conduitGet getReq =$= process sas =$= conduitPut putResp $$ sinkHandle stdout
  where
    getReq = get :: Get ClangRequest
    putResp p = put p
    process sas = do
      req <- await
      case req of
        Just (Analyze ci) -> do results <- liftIO $ doAnalyze sas ci
                                when (isJust results) $
                                  mapM_ (yield . FoundDef) (fromJust results)
                                yield EndOfDefs
                                process sas
        Just Shutdown     -> liftIO (putStrLn "Shutting down clang analysis process") >> return ()
        Nothing           -> liftIO (putStrLn "Clang analysis process encountered an error") >> return ()

doAnalyze :: SourceAnalysisState -> CommandInfo -> IO (Maybe [DefInfo])
doAnalyze sas ci = do
  liftIO $ putStrLn $ "Analyzing " ++ (show . ciSourceFile $ ci) ++ " [" ++ (show . ciBuildTime $ ci) ++ "]"
  result <- liftIO $ runSourceAnalyses sas ci
  case result of
    Just (_, ds) -> return $! (Just ds)
    Nothing      -> return $! Nothing
