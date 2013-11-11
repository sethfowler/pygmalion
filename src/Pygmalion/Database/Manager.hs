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
               | DBGetInclusions SourceFile (Response [SourceFile])
               | DBGetIncluders SourceFile (Response [SourceFile])
               | DBGetIncluderInfo SourceFile (Response [CommandInfo])
               | DBGetInclusionHierarchy SourceFile (Response String)
               | DBGetCallers SourceLocation (Response [Invocation])
               | DBGetCallees SourceLocation (Response [DefInfo])
               | DBGetBases SourceLocation (Response [DefInfo])
               | DBGetOverrides SourceLocation (Response [DefInfo])
               | DBGetMembers SourceLocation (Response [DefInfo])
               | DBGetRefs SourceLocation (Response [SourceReference])
               | DBGetReferenced SourceLocation (Response (Maybe SourceReferenced))
               | DBGetDeclReferenced SourceLocation (Response [DefInfo])
               | DBGetHierarchy SourceLocation (Response String)
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
      logInfo $ "Handled 1000 records in " ++ show (stop `diffUTCTime` start)
      newStart <- getCurrentTime
      go 0 newStart h
    go !n !s !h = {-# SCC "databaseThread" #-}
           do !req <- readLenChanPreferFirst queryChan chan
              case req of
                DBShutdown -> logInfo "Shutting down DB thread"
                _          -> route h req >> go (n+1) s h
                
route :: DBHandle -> DBRequest -> IO ()
route h (DBUpdateCommandInfo !ci)        = update "command info" updateSourceFile h ci
route h (DBUpdateDef !di)                = update "definition" updateDef h di
route h (DBUpdateOverride !ov)           = update "override" updateOverride h ov
route h (DBUpdateRef !rf)                = update "reference" updateReference h rf
route h (DBInsertFileAndCheck !sf !v)    = query "file and inserting" insertFileAndCheck h sf v
route h (DBUpdateInclusion !ic)          = update "inclusion" updateInclusion h ic
route h (DBResetMetadata !sf)            = update "resetted metadata" resetMetadata h sf
route h (DBGetCommandInfo !f !v)         = query "command info" getCommandInfo h f v
route h (DBGetSimilarCommandInfo !f !v)  = getSimilarCommandInfoQuery h f v
route h (DBGetDefinition !sl !v)         = query "definition" getDef h sl v
route h (DBGetInclusions !sf !v)         = query "inclusions" getInclusions h sf v
route h (DBGetIncluders !sf !v)          = query "includers" getIncluders h sf v
route h (DBGetIncluderInfo !sf !v)       = query "includer info" getIncluderInfo h sf v
route h (DBGetInclusionHierarchy !sf !v) = query "inclusion hierarchy"
                                                 getInclusionHierarchy h sf v
route h (DBGetCallers !sl !v)            = query "callers" getCallers h sl v
route h (DBGetCallees !usr !v)           = query "callees" getCallees h usr v
route h (DBGetBases !usr !v)             = query "bases" getOverrided h usr v
route h (DBGetOverrides !usr !v)         = query "overrides" getOverriders h usr v
route h (DBGetMembers !usr !v)           = query "members" getMembers h usr v
route h (DBGetRefs !usr !v)              = query "references" getReferences h usr v
route h (DBGetReferenced !sl !v)         = query "referenced" getReferenced h sl v
route h (DBGetDeclReferenced !sl !v)     = query "decl referenced" getDeclReferenced h sl v
route h (DBGetHierarchy !sl !v)          = query "hierarchy" getHierarchy h sl v
route _ (DBShutdown)                     = error "Should not route DBShutdown"

update :: Show a => String -> (DBHandle -> a -> IO ()) -> DBHandle -> a -> IO ()
update item f h x = do
  logDebug $ "Updating index with " ++ item ++ ": " ++ (show x)
  f h x

query :: Show a => String -> (DBHandle -> a -> IO b) -> DBHandle -> a -> Response b -> IO ()
query item f h x r = do
  logDebug $ "Getting " ++ item ++ " for " ++ (show x)
  sendResponse r =<< f h x

-- TODO: Remove this special case.
getSimilarCommandInfoQuery :: DBHandle -> SourceFile -> Response (Maybe CommandInfo) -> IO ()
getSimilarCommandInfoQuery h f v = do
  logDebug $ "Getting similar CommandInfo for " ++ (show f)
  ci <- liftM2 (<|>) (getCommandInfo h f) (getSimilarCommandInfo h f)
  sendResponse v ci
