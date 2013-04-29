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
import System.Posix.Process

import Pygmalion.Analysis.ClangRequest
import Pygmalion.Analysis.Source
import Pygmalion.Core
import Pygmalion.Log

main :: IO ()
main = do
    initLogger INFO
    logDebug "Starting clang analysis process"
    nice 10
    wd <- T.pack <$> getCurrentDirectory
    sas <- mkSourceAnalysisState wd
    runResourceT (sourceHandle stdin $= conduitGet getReq =$= process sas =$= conduitPut putResp $$ sinkHandle stdout)
  where
    getReq = get :: Get ClangRequest
    putResp :: Putter ClangResponse
    putResp p = put p
    process :: SourceAnalysisState -> Conduit ClangRequest (ResourceT IO) ClangResponse
    process sas = do
      req <- await
      case req of
        Just (Analyze ci) -> do maySar <- liftIO $ doAnalyze sas ci
                                when (isJust maySar) $ do
                                  let sar = fromJust maySar
                                  mapM_ (yield . FoundInclusion) (sarInclusions sar)
                                  mapM_ (yield . FoundDef) (sarDefs sar)
                                  mapM_ (yield . FoundOverride) (sarOverrides sar)
                                  mapM_ (yield . FoundRef) (sarRefs sar)
                                yield EndOfAnalysis
                                process sas
        Just Shutdown     -> liftIO (logDebug "Shutting down clang analysis process") >> return ()
        Nothing           -> liftIO (logError "Clang analysis process encountered an error") >> return ()

doAnalyze :: SourceAnalysisState -> CommandInfo -> IO (Maybe SourceAnalysisResult)
doAnalyze sas ci = do
  logDebug $ "Analyzing " ++ (show . ciSourceFile $ ci)
  runSourceAnalyses sas ci
