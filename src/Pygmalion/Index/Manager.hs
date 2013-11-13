{-# LANGUAGE OverloadedStrings #-}

module Pygmalion.Index.Manager
( runIndexManager
) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Data.Time.Clock.POSIX
import System.Directory
import System.Exit
import System.Process

import Control.Concurrent.Chan.Len
import Pygmalion.Config
import Pygmalion.Core
import Pygmalion.Database.Request
import Pygmalion.Index.Extension
import Pygmalion.Index.Request
import Pygmalion.Index.Stream
import Pygmalion.Log

runIndexManager :: Config -> DBChan -> DBChan -> IndexStream -> IO ()
runIndexManager cf dbChan dbQueryChan is = go
  where
    ctx = IndexContext (ifPort cf) (idxCmd cf) is dbChan dbQueryChan
    go = {-# SCC "indexThread" #-} do
         req <- atomically $ getNextFileToIndex is
         case req of
             Index r t -> do runReaderT (indexIfDirty r t) ctx
                             atomically $ finishIndexingFile (icIndexStream ctx) r
                             go
             Shutdown  -> logInfo "Shutting down indexing thread"

data IndexContext = IndexContext
  { icPort         :: !Port
  , icIndexer      :: !String
  , icIndexStream  :: !IndexStream
  , icDBChan       :: !DBChan
  , icDBQueryChan  :: !DBChan
  }
type Indexer a = ReaderT IndexContext IO a

indexIfDirty :: IndexRequest -> Maybe Time -> Indexer ()
indexIfDirty req lastIndexedTime =
  -- If this is a FromDepChange request and we have a last indexed time,
  -- we can potentially bail without even reading the file's mtime.
  case (req, lastIndexedTime) of
    (FromDepChange ci t, Just lastT)
      | lastT < t -> index (ciLastMTime ci) =<< reset ci
      | otherwise -> ignoreUnchanged req lastT True
    _             -> indexIfDirty' req

indexIfDirty' :: IndexRequest -> Indexer ()
indexIfDirty' req = do
  mayMTime <- getMTime $ reqSF req

  -- Bail early if we couldn't read the file.
  case mayMTime of
    Just mtime -> indexIfDirty'' req mtime
    Nothing    -> ignoreUnreadable req

indexIfDirty'' :: IndexRequest -> Time -> Indexer ()
indexIfDirty'' req mtime = do
  ctx <- ask
  mayOldCI <- callLenChan (icDBQueryChan ctx) $ DBGetCommandInfo (reqSF req)

  case (req, mayOldCI) of
    (FromBuild ci, Just oldCI)
      | commandInfoChanged ci oldCI  -> index mtime =<< reset =<< checkDeps mtime ci
      | ciLastMTime oldCI /= mtime   -> index mtime =<< reset =<< checkDeps mtime ci
      | otherwise                    -> ignoreUnchanged req mtime False
    (FromBuild ci, Nothing)          -> index mtime ci
    (FromNotify _, Just oldCI)
      | ciLastMTime oldCI /= mtime   -> index mtime =<< reset =<< checkDeps mtime oldCI
      | otherwise                    -> ignoreUnchanged req mtime False
    (FromNotify _, Nothing)          -> ignoreUnknown req
    (FromDepChange ci t, Just oldCI)
      | ciLastIndexed oldCI < t      -> index mtime =<< reset ci
      | otherwise                    -> ignoreUnchanged req (ciLastIndexed oldCI) False
    (FromDepChange ci _, Nothing)    -> index mtime =<< reset ci

commandInfoChanged :: CommandInfo -> CommandInfo -> Bool
commandInfoChanged a b | ciWorkingPath a /= ciWorkingPath b = True
                       | ciCommand a     /= ciCommand b     = True
                       | ciArgs a        /= ciArgs b        = True
                       | ciLanguage a    /= ciLanguage b    = True
                       | otherwise                          = False

checkDeps :: Time -> CommandInfo -> Indexer CommandInfo
checkDeps mtime ci = do
  when (hasHeaderExtensionBS $ ciSourceFile ci) $ do
    ctx <- ask
    others <- otherFilesToReindex ci
    forM_ others $ \f ->
      lift $ atomically $ addPendingIndex (icIndexStream ctx) (FromDepChange f mtime)
  return ci

reset :: CommandInfo -> Indexer CommandInfo
reset ci = do
  ctx <- ask
  writeLenChan (icDBChan ctx) (DBResetMetadata $ ciSourceFile ci)
  return ci

index :: Time -> CommandInfo -> Indexer ()
index mtime ci = go 1
  where
    go :: Int -> Indexer ()
    go retries = do
      ctx <- ask
      let is = icIndexStream ctx
      let sf = ciSourceFile ci
      time <- floor <$> lift getPOSIXTime

      -- Do the actual indexing.
      logInfo $ "Indexing " ++ show sf
      (_, _, _, h) <- lift $ createProcess
                           (proc (icIndexer ctx) [show (icPort ctx), show ci])
      code <- lift $ waitForProcess h

      -- Update the last indexed time.
      case (code, retries) of
        (ExitSuccess, _) -> do lift $ atomically $ updateLastIndexedCache is sf time 
                               updateCommand $ ci { ciLastMTime = mtime, ciLastIndexed = time }
        (_, 0)           -> do logInfo "Indexing process failed."
                               -- Make sure we reindex next time.
                               lift $ atomically $ updateLastIndexedCache is sf 0 
                               updateCommand $ ci { ciLastMTime = 0, ciLastIndexed = 0 }
        (_, _)           -> do logInfo "Indexing process failed; will retry..."
                               go (retries - 1)

ignoreUnchanged :: IndexRequest -> Time -> Bool -> Indexer ()
ignoreUnchanged req mtime cached = logInfo $ "Index is up-to-date for file "
                                          ++ (show . reqSF $ req)
                                          ++ " (file mtime: " ++ show mtime ++ ")"
                                          ++ (if cached then " (cached)" else "")

ignoreUnknown :: IndexRequest -> Indexer ()
ignoreUnknown req = logInfo $ "Not indexing unknown file "
                           ++ (show . reqSF $ req)

ignoreUnreadable :: IndexRequest -> Indexer ()
ignoreUnreadable req = logInfo $ "Not indexing unreadable file "
                              ++ (show . reqSF $ req)

-- If the source file associated with this CommandInfo has changed, what must
-- we reindex?
otherFilesToReindex :: CommandInfo -> Indexer [CommandInfo]
otherFilesToReindex ci = do
  ctx <- ask
  callLenChan (icDBQueryChan ctx) $ DBGetIncluderInfo (ciSourceFile ci)

updateCommand :: CommandInfo -> Indexer ()
updateCommand ci = do
  ctx <- ask
  writeLenChan (icDBChan ctx) (DBUpdateCommandInfo ci)

getMTime :: SourceFile -> Indexer (Maybe Time)
getMTime sf = lift $ do
  result <- try $ getModificationTime (unSourceFile sf)
  case result of
    Right clockTime -> return . Just . floor . utcTimeToPOSIXSeconds $ clockTime
    Left e          -> do logInfo $ "Couldn't read mtime for file "
                                 ++ unSourceFile sf ++ ": "
                                 ++ show (e :: IOException)
                          return Nothing  -- Most likely the file has been deleted.
