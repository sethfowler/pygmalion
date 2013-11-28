{-# LANGUAGE OverloadedStrings #-}

module Pygmalion.Index.Manager
( runIndexManager
) where

import Control.Concurrent.STM
import Control.Monad.Reader
import System.Exit
import System.Process

import Control.Concurrent.Chan.Len
import Pygmalion.Config
import Pygmalion.Core
import Pygmalion.Database.Request
import Pygmalion.File
import Pygmalion.Index.Request
import Pygmalion.Index.Stream
import Pygmalion.Log

runIndexManager :: Config -> DBUpdateChan -> DBQueryChan -> IndexStream -> IO ()
runIndexManager cf dbUpdateChan dbQueryChan is = go
  where
    ctx = IndexContext (ifPort cf) (idxCmd cf) is dbUpdateChan dbQueryChan
    go = do
         req <- atomically $ getNextFileToIndex is
         case req of
             Index r -> do
               flip runReaderT ctx $ do
                dirtiness <- fileDirtiness r
                logDirtiness r dirtiness
                case dirtiness of
                  NewFile ci t        -> updateCI ci >> updateFile r t >> index ci
                  FileChanged ci t    -> checkDeps r >> reset ci >> updateFile r t >> index ci
                  CommandChanged ci   -> checkDeps r >> reset ci >> updateCI ci >> index ci
                  CheckDepsOnly t     -> checkDeps r >> updateFile r t
                  FileDepChanged ci   -> checkDeps r >> reset ci >> index ci
                  FileUnchanged       -> ignoreUnchanged
                  FileUnreadable      -> ignoreUnreadable r
                  FileUnknown         -> ignoreUnknown r
               atomically $ finishIndexingFile (icIndexStream ctx) (reqSF r)
               go
             Shutdown -> logInfo "Shutting down indexing thread"

data IndexContext = IndexContext
  { icPort         :: !Port
  , icIndexer      :: !String
  , icIndexStream  :: !IndexStream
  , icDBUpdateChan :: !DBUpdateChan
  , icDBQueryChan  :: !DBQueryChan
  }
type Indexer a = ReaderT IndexContext IO a

data FileDirtiness = NewFile CommandInfo Time
                   | FileChanged CommandInfo Time
                   | CommandChanged CommandInfo
                   | CheckDepsOnly Time
                   | FileDepChanged CommandInfo
                   | FileUnchanged
                   | FileUnreadable
                   | FileUnknown
                     deriving (Eq, Show)
  
-- Trigger indexing if this file is dirty.
fileDirtiness :: IndexRequest -> Indexer FileDirtiness
fileDirtiness req = do
    ctx <- ask
    mayDirtiness <- lift $ atomically $ dirtinessCacheLookup (icIndexStream ctx) (reqSF req)
    mayLastCI <- lift $ atomically $ commandInfoCacheLookup (icIndexStream ctx) (reqSF req)
    case (req, mayDirtiness, mayLastCI) of
      (IndexAdd ci, Just HasNotChanged, Just lastCI)
        | ci /= lastCI                                 -> return $ CommandChanged ci
        | otherwise                                    -> return FileUnchanged
      (IndexUpdate _, Just HasChangedDep, Just lastCI) -> return $ FileDepChanged lastCI
      (_, Just HasChangedDep, _)                       -> fileDirtiness' True
      _                                                -> fileDirtiness' False
  where
    fileDirtiness' hasChangedDep = do
      ctx <- ask
      mayMTime <- lift $ getMTime $ reqSF req
      info <- callLenChan (icDBQueryChan ctx) $ DBGetCommandInfo (reqSF req)
      case (req, info, mayMTime) of
        (_, _, Nothing)                               -> return FileUnreadable
        (IndexUpdate _, (Nothing, Nothing), _)        -> return FileUnknown
        (IndexUpdate _, (Nothing, Just oldMTime), Just mtime)
          | oldMTime /= mtime                         -> return $ CheckDepsOnly mtime
          | hasChangedDep                             -> return $ CheckDepsOnly mtime
          | otherwise                                 -> return FileUnchanged
        (IndexUpdate _, (Just oldCI, Just oldMTime), Just mtime)
          | oldMTime /= mtime                         -> return $ FileChanged oldCI mtime
          | hasChangedDep                             -> return $ FileDepChanged oldCI
          | otherwise                                 -> return FileUnchanged
        (IndexAdd ci, (Nothing, Nothing), Just mtime) -> return $ NewFile ci mtime
        (IndexAdd ci, (Just oldCI, Just oldMTime), Just mtime)
          | oldMTime /= mtime                         -> return $ FileChanged ci mtime
          | ci /= oldCI                               -> return $ CommandChanged ci
          | hasChangedDep                             -> return $ FileDepChanged ci
          | otherwise                                 -> return FileUnchanged
        (_, _, _)                                     -> do logWarn $
                                                              "Unexpected dirtiness for " ++
                                                              show (reqSF req)
                                                            return FileUnknown

logDirtiness :: IndexRequest -> FileDirtiness -> Indexer ()
logDirtiness r (NewFile _ _)      = logInfo $ "Indexing new file " ++ show (reqSF r)
logDirtiness r (FileChanged _ _)  = logInfo $ "Indexing changed file " ++ show (reqSF r)
logDirtiness r (CommandChanged _) = logInfo $ "Indexing file with changed command "
                                           ++ show (reqSF r)
logDirtiness r (CheckDepsOnly _)  = logInfo $ "Checking deps only for file " ++ show (reqSF r)
logDirtiness r (FileDepChanged _) = logInfo $ "Indexing file with changed dep "
                                           ++ show (reqSF r)
logDirtiness r FileUnchanged      = logInfo $ "Not indexing unchanged file " ++ show (reqSF r)
logDirtiness r FileUnreadable     = logInfo $ "Not indexing unreadable file " ++ show (reqSF r)
logDirtiness r FileUnknown        = logInfo $ "Not indexing unknown file " ++ show (reqSF r)

checkDeps :: IndexRequest -> Indexer ()
checkDeps req = do
  ctx <- ask
  others <- callLenChan (icDBQueryChan ctx) $ DBGetDirectIncluders (reqSF req)
  forM_ others $ \f -> do
    lift $ putStrLn $ "Adding a pending update for dep " ++ show f
    lift $ atomically $ addPendingDepIndex (icIndexStream ctx) (IndexUpdate f)

updateFile :: IndexRequest -> Time -> Indexer ()
updateFile req t = do
  ctx <- ask
  let sf = reqSF req
  lift $ atomically $ updateDirtinessCache (icIndexStream ctx) sf HasNotChanged
  writeLenChan (icDBUpdateChan ctx) [DBUpdateFile sf t]

updateCI :: CommandInfo -> Indexer ()
updateCI ci = do
  ctx <- ask
  lift $ atomically $ updateDirtinessCache (icIndexStream ctx) (ciSourceFile ci) HasNotChanged
  lift $ atomically $ updateCommandInfoCache (icIndexStream ctx) ci
  writeLenChan (icDBUpdateChan ctx) [DBUpdateCommandInfo ci]

reset :: CommandInfo -> Indexer ()
reset ci = do
  ctx <- ask
  writeLenChan (icDBUpdateChan ctx) [DBResetMetadata $ ciSourceFile ci]

index :: CommandInfo -> Indexer ()
index ci = go 1
  where
    go :: Int -> Indexer ()
    go retries = do
      ctx <- ask
      let sf = ciSourceFile ci
          
      (_, _, _, h) <- lift $ createProcess
                           (proc (icIndexer ctx) [show (icPort ctx), show ci])
      code <- lift $ waitForProcess h

      case (code, retries) of
        (ExitSuccess, _) -> return ()
        (_, 0)           -> logInfo $ "Indexing process failed for " ++ show sf ++ "."
        (_, _)           -> logInfo "Indexing process failed; will retry..." >> go (retries - 1)

ignoreUnchanged :: Indexer ()
ignoreUnchanged = return ()

ignoreUnknown :: IndexRequest -> Indexer ()
ignoreUnknown req = logInfo $ "Not indexing unknown file "
                           ++ (show . reqSF $ req)

ignoreUnreadable :: IndexRequest -> Indexer ()
ignoreUnreadable req = logInfo $ "Not indexing unreadable file "
                              ++ (show . reqSF $ req)
