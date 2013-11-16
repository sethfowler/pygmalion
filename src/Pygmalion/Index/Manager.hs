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
             Index r  -> do ci <- atomically $ getLastIndexedCache is r
                            runReaderT (indexIfDirty r ci) ctx
                            atomically $ finishIndexingFile (icIndexStream ctx) r
                            go
             Shutdown -> logInfo "Shutting down indexing thread"

data IndexContext = IndexContext
  { icPort         :: !Port
  , icIndexer      :: !String
  , icIndexStream  :: !IndexStream
  , icDBChan       :: !DBChan
  , icDBQueryChan  :: !DBChan
  }
type Indexer a = ReaderT IndexContext IO a

-- Trigger indexing if this file is dirty. This first version of the
-- function makes whatever decisions we can make without hitting the
-- disk or the database.
indexIfDirty :: IndexRequest -> Maybe CommandInfo -> Indexer ()
indexIfDirty req mayLastCI =
  -- In general if we have a cached CI we make a decision here as long
  -- as we haven't seen a FromNotify for the file.
  case (req, mayLastCI) of
    (FromBuild ci False, Just lastCI)
      | commandInfoChanged ci lastCI -> index (ciLastMTime lastCI) =<<
                                        reset =<<
                                        checkDeps (ciLastMTime lastCI) lastCI
      | otherwise                    -> ignoreUnchanged req (ciLastMTime lastCI) True
    (FromDepChange ci t False, Just lastCI)
      | ciLastIndexed lastCI < t -> index (ciLastMTime lastCI) =<< reset ci
      | otherwise                -> ignoreUnchanged req (ciLastIndexed lastCI) True
    _                            -> indexIfDirty' req mayLastCI

-- This second version of the function makes the decisions we can make
-- where reading the mtime is required, but hitting the database isn't.
indexIfDirty' :: IndexRequest -> Maybe CommandInfo -> Indexer ()
indexIfDirty' req mayLastCI = do
  mayMTime <- getMTime $ reqSF req

  case (req, mayLastCI, mayMTime) of
    (_, _, Nothing)    -> ignoreUnreadable req
    (FromBuild ci True, Just lastCI, Just mtime)
      | commandInfoChanged ci lastCI -> index mtime =<< reset =<< checkDeps mtime ci
      | ciLastMTime lastCI /= mtime  -> index mtime =<< reset =<< checkDeps mtime ci
      | otherwise                    -> ignoreUnchanged req mtime True
    (FromNotify _, Just lastCI, Just mtime)
      | ciLastMTime lastCI /= mtime  -> index mtime =<< reset =<< checkDeps mtime lastCI
      | otherwise                    -> ignoreUnchanged req mtime True
    (FromDepChange ci t True, Just lastCI, Just mtime)
      | ciLastIndexed lastCI < t     -> index mtime =<< reset ci
      | ciLastMTime lastCI /= mtime  -> index mtime =<< reset ci
      | otherwise                    -> ignoreUnchanged req (ciLastIndexed lastCI) True
    (_, _, Just mtime)               -> indexIfDirty'' req mtime

-- This final version of the function is used only if we don't get a
-- cache hit. We're forced to hit the database to make a final decision.
indexIfDirty'' :: IndexRequest -> Time -> Indexer ()
indexIfDirty'' req mtime = do
  ctx <- ask
  mayOldCI <- callLenChan (icDBQueryChan ctx) $ DBGetCommandInfo (reqSF req)

  case (req, mayOldCI) of
    (FromBuild ci _, Just oldCI)
      | commandInfoChanged ci oldCI  -> index "iid'' FromBuild ci" mtime =<< reset =<< checkDeps mtime ci
      | ciLastMTime oldCI /= mtime   -> index "iid'' FromBuild mt" mtime =<< reset =<< checkDeps mtime ci
      | otherwise                    -> ignoreUnchanged req mtime False
    (FromBuild ci _, Nothing)        -> index "iid'' FromBuild new" mtime ci
    (FromNotify _, Just oldCI)
      | ciLastMTime oldCI /= mtime   -> index "iid'' FromNotify mt" mtime =<< reset =<< checkDeps mtime oldCI
      | otherwise                    -> ignoreUnchanged req mtime False
    (FromNotify _, Nothing)          -> ignoreUnknown req
    (FromDepChange ci t _, Just oldCI)
      | ciLastIndexed oldCI < t      -> index "iid'' FromDepChange li" mtime =<< reset ci
      | ciLastMTime oldCI /= mtime   -> index "iid'' FromDepChange mt" mtime =<< reset ci
      | otherwise                    -> ignoreUnchanged req (ciLastIndexed oldCI) False
    (FromDepChange ci _ _, Nothing)  -> index "iid'' FromDepChange new" mtime =<< reset ci

commandInfoChanged :: CommandInfo -> CommandInfo -> Bool
commandInfoChanged a b | hasHeaderExtensionBS (ciSourceFile a) = False  -- Hack until we modify how inclusions work.
                       | ciWorkingPath a /= ciWorkingPath b = True
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
      lift $ atomically $ addPendingIndex (icIndexStream ctx) (FromDepChange f mtime False)
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
        (ExitSuccess, _) -> do let newCI = ci { ciLastMTime = mtime, ciLastIndexed = time }
                               lift $ atomically $ updateLastIndexedCache is newCI
                               updateCommand newCI
        (_, 0)           -> do logInfo "Indexing process failed."
                               -- Make sure we reindex next time.
                               let newCI = ci { ciLastMTime = 0, ciLastIndexed = 0 }
                               lift $ atomically $ updateLastIndexedCache is newCI
                               updateCommand newCI
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
