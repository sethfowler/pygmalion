module Pygmalion.Analysis.Manager
( runAnalysisManager
, AnalysisRequest (..)
, AnalysisChan
) where

import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Data.ByteString.Char8 (ByteString)
import Data.Conduit
import Data.Conduit.Cereal
import Data.Conduit.Process
import Data.DateTime
import Data.Maybe
import Data.Time.Clock.POSIX
import Data.Serialize
import System.Directory

import Control.Concurrent.Chan.Len
import qualified Pygmalion.Analysis.ClangRequest as CR
import Pygmalion.Analysis.Extension
import Pygmalion.Core
import Pygmalion.Database.Manager
import Pygmalion.Log

data AnalysisRequest = AnalyzeBuiltFile CommandInfo
                     | AnalyzeNotifiedFile SourceFile
                     | ShutdownAnalysis
type AnalysisChan = LenChan AnalysisRequest

runAnalysisManager :: AnalysisChan -> DBChan -> DBChan -> IO ()
runAnalysisManager aChan dbChan dbQueryChan = do
    let indexer = conduitProcess (proc "pygclangindex" []) :: Indexer
    go indexer
  where
    go indexer = {-# SCC "analysisThread" #-} do
                  (newCount, req) <- readLenChan aChan
                  logDebug $ "Analysis channel now has " ++ (show newCount) ++ " items waiting"
                  case req of
                      AnalyzeBuiltFile cmd   -> doAnalyzeBuiltFile dbChan dbQueryChan indexer cmd >> go indexer
                      AnalyzeNotifiedFile sf -> doAnalyzeNotifiedFile dbChan dbQueryChan indexer sf >> go indexer
                      ShutdownAnalysis -> logInfo "Shutting down analysis thread"

type Indexer = Conduit ByteString (ResourceT IO) ByteString

getMTime :: SourceFile -> IO Time
getMTime sf = do
  result <- try $ getModificationTime (unSourceFile sf)
  case result of
    Right clockTime -> return $ floor (utcTimeToPOSIXSeconds . fromClockTime $ clockTime)
    Left e          -> do logInfo $ "Couldn't read mtime for file " ++ (unSourceFile sf) ++ ": " ++ (show (e :: IOException))
                          return 0  -- Most likely the file has been deleted.

doAnalyzeBuiltFile :: DBChan -> DBChan -> Indexer -> CommandInfo -> IO ()
doAnalyzeBuiltFile dbChan dbQueryChan indexer ci = do
    -- FIXME: Add an abstraction over this pattern.
    oldCI <- callLenChan dbQueryChan $ DBGetCommandInfo (ciSourceFile ci)
    mtime <- getMTime (ciSourceFile ci)
    when (isJust oldCI) $ logInfo ("Deciding whether to analyze file with index time: " ++ (show . ciLastIndexed . fromJust $ oldCI))
    case oldCI of
      Just oldCI' | (ciLastIndexed oldCI') < mtime -> doAnalyze'
                  | otherwise                      -> do updateCommand dbChan ci
                                                         logInfo $ "Index is up-to-date for built file " ++ (show . ciSourceFile $ ci) ++ " (file mtime: " ++ (show mtime) ++ ")"
      _                                            -> doAnalyze'
  where
    doAnalyze' = do toReindex <- filesToReindex dbQueryChan ci
                    forM_ toReindex $ \f -> analyzeCode dbChan indexer f
                    updateCommand dbChan ci

doAnalyzeNotifiedFile :: DBChan -> DBChan -> Indexer -> SourceFile -> IO ()
doAnalyzeNotifiedFile dbChan dbQueryChan indexer sf = do
    oldCI <- callLenChan dbQueryChan $ DBGetCommandInfo sf
    case oldCI of
      Just oldCI' -> do mtime <- getMTime (ciSourceFile oldCI')
                        doAnalyzeSource' oldCI' mtime
      _           -> logInfo $ "Not indexing unknown file " ++ (show sf)
  where
    doAnalyzeSource' ci mt | (ciLastIndexed ci) < mt = do toReindex <- filesToReindex dbQueryChan ci
                                                          forM_ toReindex $ \f -> analyzeCode dbChan indexer f
                           | otherwise               = logInfo $ "Index is up-to-date for file " ++ (show sf)

-- If the source file associated with this CommandInfo has changed, what must
-- we reindex?
filesToReindex :: DBChan -> CommandInfo -> IO [CommandInfo]
filesToReindex dbQueryChan ci = do
  includers <- callLenChan dbQueryChan $ DBGetIncluders (ciSourceFile ci)
  case hasHeaderExtensionText (ciSourceFile ci) of
    True ->  return includers  -- We don't reindex the header file itself.
    False -> return (ci : includers)

updateCommand :: DBChan -> CommandInfo -> IO ()
updateCommand dbChan ci = writeLenChan dbChan (DBUpdateCommandInfo ci)

analyzeCode :: DBChan -> Indexer -> CommandInfo -> IO ()
analyzeCode dbChan indexer ci = do
    logInfo $ "Indexing " ++ (show sf)
    writeLenChan dbChan (DBResetInclusions sf)
    runResourceT (source $= conduitPut putReq =$= indexer =$= conduitGet getResp $$ process)
  where
    sf = ciSourceFile ci
    source = yield (CR.Analyze ci) >> yield (CR.Shutdown)
    putReq :: Putter CR.ClangRequest
    putReq = put
    getResp = get :: Get CR.ClangResponse
    process = do
      liftIO $ logDebug "WAITING"
      resp <- await
      case resp of
        Just (CR.FoundDef di)       -> liftIO (writeLenChan dbChan (DBUpdateDefInfo di)) >> process
        Just (CR.FoundOverride ov)  -> liftIO (writeLenChan dbChan (DBUpdateOverride ov)) >> process
        Just (CR.FoundRef rf)       -> liftIO (writeLenChan dbChan (DBUpdateRef rf)) >> process
        Just (CR.FoundInclusion ic) -> liftIO (writeLenChan dbChan (DBUpdateInclusion ci ic)) >> process
        Just (CR.EndOfInclusions)   -> liftIO (writeLenChan dbChan (DBResetMetadata sf)) >> process
        Just (CR.EndOfAnalysis)     -> liftIO (logDebug "Done reading from clang process") >> return ()
        Nothing                     -> liftIO (logDebug "Clang process read failed") >> return ()
