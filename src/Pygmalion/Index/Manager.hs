{-# LANGUAGE OverloadedStrings #-}

module Pygmalion.Index.Manager
( runIndexManager
) where

import Control.Applicative
import Control.Concurrent.STM
--import Control.Monad
import Control.Monad.Reader
import Data.Time.Clock.POSIX
import System.Exit
import System.Process

import Control.Concurrent.Chan.Len
import Pygmalion.Config
import Pygmalion.Core
import Pygmalion.Database.Request
--import Pygmalion.Index.Extension
import Pygmalion.Index.Request
import Pygmalion.Index.Stream
import Pygmalion.Log

runIndexManager :: Config -> DBUpdateChan -> DBQueryChan -> IndexStream -> IO ()
runIndexManager cf dbUpdateChan dbQueryChan is = go
  where
    ctx = IndexContext (ifPort cf) (idxCmd cf) is dbUpdateChan --dbQueryChan
    getCI sfHash = callLenChan dbQueryChan $ DBGetCommandInfo sfHash
    go = {-# SCC "indexThread" #-} do
         req <- atomically $ getNextFileToIndex is
         case req of
             Index r -> do
               mayCI <- atomically $ getLastIndexedCache is (reqSF r)
               dirtiness <- fileDirtiness r (getCI . reqSF $ r) mayCI
               flip runReaderT ctx $
                case dirtiness of
                  NewFile t ci           -> index t ci
                  NewInclusion _         -> error "Shouldn't get NewInclusion here"
                  CommandChanged t ci    -> checkDeps t ci >>= reset >>= index t
                  FileChanged t ci       -> checkDeps t ci >>= reset >>= index t
                  DependencyChanged t ci -> reset ci >>= index t
                  InclusionChanged _     -> error "Shouldn't get InclusionChanged here"
                  Unchanged _            -> ignoreUnchanged
                  Unreadable             -> ignoreUnreadable r
                  Unknown                -> ignoreUnknown r
               atomically $ finishIndexingFile (icIndexStream ctx) (reqSF r)
               go
             Shutdown -> logInfo "Shutting down indexing thread"

data IndexContext = IndexContext
  { icPort         :: !Port
  , icIndexer      :: !String
  , icIndexStream  :: !IndexStream
  , icDBUpdateChan :: !DBUpdateChan
  --, icDBQueryChan  :: !DBQueryChan
  }
type Indexer a = ReaderT IndexContext IO a

checkDeps :: Time -> CommandInfo -> Indexer CommandInfo
checkDeps _ ci = return ci
{-
checkDeps mtime ci = do
  unless (hasSourceExtension $ ciSourceFile ci) $ do
    ctx <- ask
    others <- otherFilesToReindex ci
    forM_ others $ \f -> do
      -- Don't even request indexing if the cache says the file isn't dirty.
      mayLastCI <- lift $ atomically $ getLastIndexedCache (icIndexStream ctx) (ciSourceFile ci)
      case mayLastCI of
        Just lastCI
          | ciLastIndexed lastCI < mtime -> indexDep f mtime
          | otherwise                    -> return ()
        Nothing                          -> indexDep f mtime
  return ci
-}

{-
indexDep :: CommandInfo -> Time -> Indexer ()
indexDep sf t = do
  ctx <- ask
  lift $ atomically $ addPendingIndex (icIndexStream ctx) (FromDepChange sf t False)
-}

reset :: CommandInfo -> Indexer CommandInfo
reset ci = do
  ctx <- ask
  writeLenChan (icDBUpdateChan ctx) [DBResetMetadata $ ciSourceFile ci]
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

--ignoreUnchanged :: IndexRequest -> Time -> Bool -> Indexer ()
--ignoreUnchanged req mtime cached = logInfo $ "Index is up-to-date for file "
--                                          ++ (show . reqSF $ req)
--                                          ++ " (file mtime: " ++ show mtime ++ ")"
--                                          ++ (if cached then " (cached)" else "")
ignoreUnchanged :: Indexer ()
ignoreUnchanged = return ()

ignoreUnknown :: IndexRequest -> Indexer ()
ignoreUnknown req = logInfo $ "Not indexing unknown file "
                           ++ (show . reqSF $ req)

ignoreUnreadable :: IndexRequest -> Indexer ()
ignoreUnreadable req = logInfo $ "Not indexing unreadable file "
                              ++ (show . reqSF $ req)

{-
-- If the source file associated with this CommandInfo has changed, what must
-- we reindex?
otherFilesToReindex :: CommandInfo -> Indexer [CommandInfo]
otherFilesToReindex ci = do
  ctx <- ask
  callLenChan (icDBQueryChan ctx) $ DBGetIncluderInfo (ciSourceFile ci)
-}

updateCommand :: CommandInfo -> Indexer ()
updateCommand ci = do
  ctx <- ask
  writeLenChan (icDBUpdateChan ctx) [DBUpdateCommandInfo ci]
