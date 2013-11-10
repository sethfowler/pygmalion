{-# LANGUAGE OverloadedStrings #-}

module Pygmalion.Index.Manager
( runIndexManager
, IndexRequest (..)
, IndexStream (..)
, mkIndexStream
, shutdownIndexStream
, addPendingIndex
) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Time.Clock.POSIX
import System.Directory
import System.Exit
import System.Process

import Control.Concurrent.Chan.Len
import Pygmalion.Config
import Pygmalion.Core
import Pygmalion.Database.Manager
import Pygmalion.Log

data IndexRequest = FromBuild     CommandInfo
                  | FromNotify    SourceFile
                  | FromDepChange CommandInfo Time
                    deriving (Show)

reqSF :: IndexRequest -> SourceFile
reqSF (FromBuild  ci)      = ciSourceFile ci
reqSF (FromNotify sf)      = sf
reqSF (FromDepChange ci _) = ciSourceFile ci

-- Combines two IndexRequests. Assumes that the first argument is 'new'
-- and the second argument is 'old'.
combineReqs :: IndexRequest -> IndexRequest -> IndexRequest
combineReqs (FromDepChange _ t) (FromBuild ci) = FromDepChange ci t
combineReqs new@(FromDepChange _ _) _          = new
combineReqs (FromBuild ci) (FromDepChange _ t) = FromDepChange ci t
combineReqs new@(FromBuild _) _                = new
combineReqs (FromNotify _) old                 = old

data IndexContext = IndexContext
  { acPort         :: !Port
  , acIndexer      :: !String
  , acIndexStream  :: !IndexStream
  , acDBChan       :: !DBChan
  , acDBQueryChan  :: !DBChan
  }
type Indexer a = ReaderT IndexContext IO a

data IndexStream = IndexStream
  { lsCurrent        :: TMVar (Set.Set SourceFile)
  , lsPending        :: TMVar (Map.Map SourceFile IndexRequest)
  , lsShouldShutdown :: TVar Bool
  }

mkIndexStream :: IO IndexStream
mkIndexStream = do
  emptySet <- newTMVarIO Set.empty
  emptyMap <- newTMVarIO Map.empty
  shouldShutdown <- newTVarIO False
  return $! IndexStream emptySet emptyMap shouldShutdown

shutdownIndexStream :: IndexStream -> STM ()
shutdownIndexStream is = writeTVar (lsShouldShutdown is) True

addPendingIndex :: IndexStream -> IndexRequest -> STM ()
addPendingIndex is req = do
  curPending <- takeTMVar (lsPending is)
  let newPending = Map.insertWith combineReqs (reqSF req) req curPending
  putTMVar (lsPending is) newPending

data IndexRequestOrShutdown = Index IndexRequest
                            | Shutdown
                              deriving (Show)

getNextFileToIndex :: IndexStream -> STM IndexRequestOrShutdown
getNextFileToIndex is = do
    shouldShutdown <- readTVar (lsShouldShutdown is)
    if shouldShutdown then return Shutdown
                      else getNext
  where
    getNext = do
      curPending <- takeTMVar (lsPending is)

      -- Retry if there's nothing available. This is the magic that lets
      -- us block until there's something to index.
      check (not $ Map.null curPending)

      -- There's something available, so grab it.
      let (sf, req) = Map.elemAt 0 curPending

      -- Make sure someone else isn't already working on it.
      curCurrent <- takeTMVar (lsCurrent is)
      check (not $ sf `Set.member` curCurrent)

      -- OK, we're good to go.
      let newCurrent = sf `Set.insert` curCurrent
      putTMVar (lsCurrent is) newCurrent
      let newPending = Map.deleteAt 0 curPending
      putTMVar (lsPending is) newPending

      return $ Index req

finishIndexingFile :: IndexStream -> IndexRequest -> STM ()
finishIndexingFile is req = do
  curCurrent <- takeTMVar (lsCurrent is)
  let newCurrent = (reqSF req) `Set.delete` curCurrent
  putTMVar (lsCurrent is) newCurrent

runIndexManager :: Config -> DBChan -> DBChan -> IndexStream -> IO ()
runIndexManager cf dbChan dbQueryChan is = go
  where
    ctx = IndexContext (ifPort cf) (idxCmd cf) is dbChan dbQueryChan
    go = {-# SCC "indexThread" #-} do
         req <- atomically $ getNextFileToIndex is
         case req of
             Index r  -> do runReaderT (analyzeIfDirty r) ctx
                            atomically $ finishIndexingFile (acIndexStream ctx) r
                            go
             Shutdown -> logInfo "Shutting down indexing thread"

getMTime :: SourceFile -> Indexer (Maybe Time)
getMTime sf = lift $ do
  result <- try $ getModificationTime (unSourceFile sf)
  case result of
    Right clockTime -> return . Just . floor . utcTimeToPOSIXSeconds $ clockTime
    Left e          -> do logInfo $ "Couldn't read mtime for file "
                                 ++ unSourceFile sf ++ ": "
                                 ++ show (e :: IOException)
                          return Nothing  -- Most likely the file has been deleted.

analyzeIfDirty :: IndexRequest -> Indexer ()
analyzeIfDirty req = do
  ctx <- ask
  let sf = reqSF req
  mayOldCI <- callLenChan (acDBQueryChan ctx) $ DBGetCommandInfo sf
  mayMTime <- getMTime sf

  case (req, mayOldCI, mayMTime) of
    (_, _, Nothing)                   -> ignoreUnreadable req
    (FromBuild ci, Just oldCI, Just mtime)
      | commandInfoChanged ci oldCI   -> analyze ci mtime
      | ciLastIndexed oldCI /= mtime  -> analyze ci mtime
      | otherwise                     -> ignoreUnchanged req mtime
    (FromBuild ci, Nothing, Just mtime)
                                      -> analyze ci mtime
    (FromNotify _, Just oldCI, Just mtime)
      | ciLastIndexed oldCI /= mtime  -> analyze oldCI mtime
      | otherwise                     -> ignoreUnchanged req mtime
    (FromNotify _, Nothing, _)        -> ignoreUnknown req
    (FromDepChange ci t, Just oldCI, Just mtime)
      | ciLastIndexed oldCI < t       -> analyze ci mtime
      | otherwise                     -> ignoreUnchanged req (ciLastIndexed oldCI)
    (FromDepChange ci _, Nothing, Just mtime)
                                      -> analyze ci mtime

commandInfoChanged :: CommandInfo -> CommandInfo -> Bool
commandInfoChanged a b | ciWorkingPath a /= ciWorkingPath b = True
                       | ciCommand a     /= ciCommand b     = True
                       | ciArgs a        /= ciArgs b        = True
                       | ciLanguage a    /= ciLanguage b    = True
                       | otherwise                          = False

analyze :: CommandInfo -> Time -> Indexer ()
analyze ci mtime = do
  ctx <- ask
  others <- otherFilesToReindex ci
  forM_ others $ \f ->
    lift $ atomically $ addPendingIndex (acIndexStream ctx) (FromDepChange f mtime)
  analyzeCode ci 1

ignoreUnchanged :: IndexRequest -> Time -> Indexer ()
ignoreUnchanged req mtime = logInfo $ "Index is up-to-date for file "
                                   ++ (show . reqSF $ req)
                                   ++ " (file mtime: " ++ show mtime ++ ")"

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
  callLenChan (acDBQueryChan ctx) $ DBGetIncluderInfo (ciSourceFile ci)

updateCommand :: CommandInfo -> Indexer ()
updateCommand ci = do
  ctx <- ask
  writeLenChan (acDBChan ctx) (DBUpdateCommandInfo ci)

analyzeCode :: CommandInfo -> Int -> Indexer ()
analyzeCode ci retries = do
  ctx <- ask
  let sf = ciSourceFile ci
  time <- floor <$> lift getPOSIXTime

  -- Do the actual indexing.
  logInfo $ "Indexing " ++ show sf
  writeLenChan (acDBChan ctx) (DBResetMetadata sf)
  (_, _, _, h) <- lift $ createProcess
                       (proc (acIndexer ctx) [show (acPort ctx), show ci])
  code <- lift $ waitForProcess h

  -- Update the last indexed time.
  case (code, retries) of
    (ExitSuccess, _) -> updateCommand $ ci { ciLastIndexed = time }
    (_, 0)           -> do logInfo "Indexing process failed."
                           -- Make sure we reindex next time.
                           updateCommand $ ci { ciLastIndexed = 0 }
    (_, _)           -> do logInfo "Indexing process failed; will retry..."
                           analyzeCode ci (retries - 1)
