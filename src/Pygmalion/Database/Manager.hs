{-# LANGUAGE BangPatterns #-}

module Pygmalion.Database.Manager
( runDatabaseManager
, ensureDB
, DBRequest (..)
, DBChan
) where

import Control.Applicative
import Control.Monad
import Data.Time.Clock

import Control.Concurrent.Chan.Len
import Pygmalion.Core
import Pygmalion.Database.IO
import Pygmalion.Log

data DBRequest = DBUpdateCommandInfo CommandInfo
               | DBUpdateDef DefUpdate
               | DBUpdateOverride Override
               | DBUpdateRef ReferenceUpdate
               | DBUpdateInclusion Inclusion
               | DBInsertFileAndCheck SourceFile (Response Bool)
               | DBResetMetadata SourceFile
               | DBGetCommandInfo SourceFile (Response (Maybe CommandInfo))
               | DBGetSimilarCommandInfo SourceFile (Response (Maybe CommandInfo))
               | DBGetDefinition SourceLocation (Response [DefInfo])
               | DBGetIncluders SourceFile (Response [CommandInfo])
               | DBGetCallers SourceLocation (Response [Invocation])
               | DBGetCallees SourceLocation (Response [DefInfo])
               | DBGetBases SourceLocation (Response [DefInfo])
               | DBGetOverrides SourceLocation (Response [DefInfo])
               | DBGetRefs SourceLocation (Response [SourceReference])
               | DBGetReferenced SourceLocation (Response (Maybe SourceReferenced))
               | DBShutdown
               deriving (Show)
type DBChan = LenChan DBRequest

runDatabaseManager :: DBChan -> DBChan -> IO ()
runDatabaseManager chan queryChan = do
    start <- getCurrentTime
    withDB (go 0 start)
  where
    go :: Int -> UTCTime -> DBHandle -> IO ()
    go 1000 !start !h = do 
      stop <- getCurrentTime
      logInfo $ "Handled 1000 records in " ++ (show $ stop `diffUTCTime` start)
      newStart <- getCurrentTime
      go 0 newStart h
    go !n !s !h = {-# SCC "databaseThread" #-}
           do (!tookFirst, !newCount, !req) <- readEitherChan n queryChan chan
              logDebug $ "Database request: " ++ (show req)
              logDebug $ if tookFirst then "Query channel now has " ++ (show newCount) ++ " queries waiting"
                                      else "Database channel now has " ++ (show newCount) ++ " requests waiting"
              case req of
                DBUpdateCommandInfo !ci       -> doUpdateCommandInfo h ci >> go (n+1) s h
                DBUpdateDef !di               -> doUpdateDef h di >> go (n+1) s h
                DBUpdateOverride !ov          -> doUpdateOverride h ov >> go (n+1) s h
                DBUpdateRef !rf               -> doUpdateRef h rf >> go (n+1) s h
                DBInsertFileAndCheck !sf !v   -> doInsertFileAndCheck h sf v >> go (n+1) s h
                DBUpdateInclusion !ic         -> doUpdateInclusion h ic >> go (n+1) s h
                DBResetMetadata !sf           -> doResetMetadata h sf >> go (n+1) s h
                DBGetCommandInfo !f !v        -> doGetCommandInfo h f v >> go (n+1) s h
                DBGetSimilarCommandInfo !f !v -> doGetSimilarCommandInfo h f v >> go (n+1) s h
                DBGetDefinition !sl !v        -> doGetDefinition h sl v >> go (n+1) s h
                DBGetIncluders !sf !v         -> doGetIncluders h sf v >> go (n+1) s h
                DBGetCallers !sl !v           -> doGetCallers h sl v >> go (n+1) s h
                DBGetCallees !usr !v          -> doGetCallees h usr v >> go (n+1) s h
                DBGetBases !usr !v            -> doGetBases h usr v >> go (n+1) s h
                DBGetOverrides !usr !v        -> doGetOverrides h usr v >> go (n+1) s h
                DBGetRefs !usr !v             -> doGetRefs h usr v >> go (n+1) s h
                DBGetReferenced !sl !v        -> doGetReferenced h sl v >> go (n+1) s h
                DBShutdown                    -> logInfo "Shutting down DB thread"

readEitherChan :: Int -> DBChan -> DBChan -> IO (Bool, Int, DBRequest)
readEitherChan n queryChan chan
  | n `rem` 10 == 0 = readLenChanPreferFirst queryChan chan
  | otherwise       = readLenChanPreferFirst chan queryChan

doUpdateCommandInfo :: DBHandle -> CommandInfo -> IO ()
doUpdateCommandInfo h ci = withTransaction h $ do
  logDebug $ "Updating database with command: " ++ (show . ciSourceFile $ ci)
  updateSourceFile h ci

doUpdateDef :: DBHandle -> DefUpdate -> IO ()
doUpdateDef h du = withTransaction h $ do
  logDebug $ "Updating database with def: " ++ (show . diuUSR $ du)
  updateDef h du

doUpdateOverride :: DBHandle -> Override -> IO ()
doUpdateOverride h ov = withTransaction h $ do
  logDebug $ "Updating database with override: " ++ (show ov)
  updateOverride h ov

doUpdateRef :: DBHandle -> ReferenceUpdate -> IO ()
doUpdateRef h ru = withTransaction h $ do
  logDebug $ "Updating database with reference: " ++ (show ru)
  updateReference h ru

doUpdateInclusion :: DBHandle -> Inclusion -> IO ()
doUpdateInclusion h ic = withTransaction h $ do
    logDebug $ "Updating database with inclusion: " ++ (show ic)
    updateInclusion h ic

doInsertFileAndCheck :: DBHandle -> SourceFile -> Response Bool -> IO ()
doInsertFileAndCheck h f v = do
  logDebug $ "Inserting file and checking for " ++ (show f)
  sendResponse v =<< insertFileAndCheck h f

doResetMetadata :: DBHandle -> SourceFile -> IO ()
doResetMetadata h sf = withTransaction h $ do
  logDebug $ "Resetting metadata for file: " ++ (show sf)
  resetMetadata h sf

doGetCommandInfo :: DBHandle -> SourceFile -> Response (Maybe CommandInfo) -> IO ()
doGetCommandInfo h f v = do
  logDebug $ "Getting CommandInfo for " ++ (show f)
  sendResponse v =<< getCommandInfo h f

doGetSimilarCommandInfo :: DBHandle -> SourceFile -> Response (Maybe CommandInfo) -> IO ()
doGetSimilarCommandInfo h f v = do
  logDebug $ "Getting similar CommandInfo for " ++ (show f)
  ci <- liftM2 (<|>) (getCommandInfo h f) (getSimilarCommandInfo h f)
  sendResponse v ci

doGetDefinition :: DBHandle -> SourceLocation -> Response [DefInfo] -> IO ()
doGetDefinition h sl v = do
  logDebug $ "Getting definition for " ++ (show sl)
  sendResponse v =<< getDef h sl

doGetIncluders :: DBHandle -> SourceFile -> Response [CommandInfo] -> IO ()
doGetIncluders h sf v = do
  logDebug $ "Getting includers for " ++ (show sf)
  sendResponse v =<< getIncluders h sf

doGetCallers :: DBHandle -> SourceLocation -> Response [Invocation] -> IO ()
doGetCallers h sl v = do
  logDebug $ "Getting callers for " ++ (show sl)
  sendResponse v =<< getCallers h sl

doGetCallees :: DBHandle -> SourceLocation -> Response [DefInfo] -> IO ()
doGetCallees h sl v = do
  logDebug $ "Getting callees for " ++ (show sl)
  sendResponse v =<< getCallees h sl

doGetBases :: DBHandle -> SourceLocation -> Response [DefInfo] -> IO ()
doGetBases h sl v = do
  logDebug $ "Getting bases for " ++ (show sl)
  sendResponse v =<< getOverrided h sl

doGetOverrides :: DBHandle -> SourceLocation -> Response [DefInfo] -> IO ()
doGetOverrides h sl v = do
  logDebug $ "Getting overrides for " ++ (show sl)
  sendResponse v =<< getOverriders h sl

doGetRefs :: DBHandle -> SourceLocation -> Response [SourceReference] -> IO ()
doGetRefs h sl v = do
  logDebug $ "Getting refs for " ++ (show sl)
  sendResponse v =<< getReferences h sl

doGetReferenced :: DBHandle -> SourceLocation -> Response (Maybe SourceReferenced) -> IO ()
doGetReferenced h sl v = do
  logDebug $ "Getting referenced for " ++ (show sl)
  sendResponse v =<< getReferenced h sl
