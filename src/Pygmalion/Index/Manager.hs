{-# LANGUAGE OverloadedStrings #-}

module Pygmalion.Index.Manager
( runIndexManager
, IndexRequest (..)
, IndexStream (..)
, mkIndexStream
, shutdownIndexStream
, addPendingIndex
, clearLastIndexedCache
) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import qualified Data.IntMap.Strict as Map
import qualified Data.IntSet as Set
import Data.Time.Clock.POSIX
import System.Directory
import System.Exit
import System.Process

import Control.Concurrent.Chan.Len
import Pygmalion.Config
import Pygmalion.Core
import Pygmalion.Database.Manager
import Pygmalion.Hash
import Pygmalion.Log

data IndexRequest = FromBuild     CommandInfo
                  | FromDepChange CommandInfo Time
                  | FromNotify    SourceFile
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
  { icPort         :: !Port
  , icIndexer      :: !String
  , icIndexStream  :: !IndexStream
  , icDBChan       :: !DBChan
  , icDBQueryChan  :: !DBChan
  }
type Indexer a = ReaderT IndexContext IO a

data IndexStream = IndexStream
  { isCurrent          :: TMVar Set.IntSet
  , isPending          :: TMVar (Map.IntMap IndexRequest)
  , isLastIndexedCache :: TMVar (Map.IntMap Time)
  , isShouldShutdown   :: TVar Bool
  }

mkIndexStream :: IO IndexStream
mkIndexStream = do
  emptySet  <- newTMVarIO Set.empty
  emptyMap  <- newTMVarIO Map.empty
  emptyMap' <- newTMVarIO Map.empty
  shouldShutdown <- newTVarIO False
  return $! IndexStream emptySet emptyMap emptyMap' shouldShutdown

shutdownIndexStream :: IndexStream -> STM ()
shutdownIndexStream is = writeTVar (isShouldShutdown is) True

addPendingIndex :: IndexStream -> IndexRequest -> STM ()
addPendingIndex is req = do
  curPending <- takeTMVar (isPending is)
  let sfHash = hashInt (reqSF req)
  let newPending = Map.insertWith combineReqs sfHash req curPending
  putTMVar (isPending is) newPending

data IndexRequestOrShutdown = Index IndexRequest (Maybe Time)
                            | Shutdown
                              deriving (Show)

getNextFileToIndex :: IndexStream -> STM IndexRequestOrShutdown
getNextFileToIndex is = do
    shouldShutdown <- readTVar (isShouldShutdown is)
    if shouldShutdown then return Shutdown
                      else getNext
  where
    getNext = do
      curPending <- takeTMVar (isPending is)

      -- Retry if there's nothing available. This is the magic that lets
      -- us block until there's something to index.
      check (not $ Map.null curPending)

      -- There's something available, so grab it.
      let (sfHash, req) = Map.findMin curPending

      -- Make sure someone else isn't already working on it.
      curCurrent <- takeTMVar (isCurrent is)
      check (not $ sfHash `Set.member` curCurrent)

      -- OK, we're good to go.
      let newCurrent = sfHash `Set.insert` curCurrent
      putTMVar (isCurrent is) newCurrent
      let newPending = Map.deleteMin curPending
      putTMVar (isPending is) newPending
      
      -- Grab a cached last indexed time if available.
      curCache <- readTMVar (isLastIndexedCache is)
      let lastIndexedTime = sfHash `Map.lookup` curCache

      return $ Index req lastIndexedTime

finishIndexingFile :: IndexStream -> IndexRequest -> STM ()
finishIndexingFile is req = do
  curCurrent <- takeTMVar (isCurrent is)
  let sfHash = hashInt (reqSF req)
  let newCurrent = sfHash `Set.delete` curCurrent
  putTMVar (isCurrent is) newCurrent

updateLastIndexedCache :: IndexStream -> SourceFile -> Time -> STM ()
updateLastIndexedCache is sf t = do
  curCache <- takeTMVar (isLastIndexedCache is)
  let newCache = Map.insert (hashInt sf) t curCache
  putTMVar (isLastIndexedCache is) newCache

clearLastIndexedCache :: IndexStream -> STM ()
clearLastIndexedCache is = void $ swapTMVar (isLastIndexedCache is) Map.empty

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
