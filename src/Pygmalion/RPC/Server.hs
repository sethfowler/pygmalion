{-# LANGUAGE OverloadedStrings #-}

module Pygmalion.RPC.Server
( runRPCServer
) where

import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString.Char8 (ByteString)
import Data.Conduit
import Data.Conduit.Cereal
import Data.Conduit.Network
import Data.Serialize
import Data.String

import Control.Concurrent.Chan.Len
import Pygmalion.Config
import Pygmalion.Core
import Pygmalion.Database.Manager
import Pygmalion.Index.Manager
import Pygmalion.Log
import Pygmalion.RPC.Request

runRPCServer :: Config -> IndexChan -> DBChan -> DBChan -> IO ()
runRPCServer cf aChan dbChan dbQueryChan =
    runTCPServer settings (serverApp aChan dbChan dbQueryChan)
  where
    settings = (serverSettings port addr) :: ServerSettings IO
    port = ifPort cf
    addr = fromString (ifAddr cf)

serverApp :: IndexChan -> DBChan -> DBChan -> Application IO
serverApp aChan dbChan dbQueryChan ad =
    (appSource ad) $= conduitGet getCI =$= process $$ (appSink ad)
  where
    getCI = {-# SCC "serverget" #-} get :: Get RPCRequest
    process = do
      result <- await
      case result of
        Just (RPCSendCommandInfo ci)       -> doSendCommandInfo aChan ci >> process
        Just (RPCGetCommandInfo sf)        -> doGetCommandInfo dbQueryChan sf >>= yield >> process
        Just (RPCGetSimilarCommandInfo sf) -> doGetSimilarCommandInfo dbQueryChan sf >>= yield >> process
        Just (RPCGetDefinition usr)        -> doGetDefinition dbQueryChan usr >>= yield >> process
        Just (RPCGetCallers usr)           -> doGetCallers dbQueryChan usr >>= yield >> process
        Just (RPCGetCallees usr)           -> doGetCallees dbQueryChan usr >>= yield >> process
        Just (RPCGetBases usr)             -> doGetBases dbQueryChan usr >>= yield >> process
        Just (RPCGetOverrides usr)         -> doGetOverrides dbQueryChan usr >>= yield >> process
        Just (RPCGetRefs usr)              -> doGetRefs dbQueryChan usr >>= yield >> process
        Just (RPCGetReferenced sl)         -> doGetReferenced dbQueryChan sl >>= yield >> process
        Just (RPCFoundDef du)              -> doFoundDef dbChan du >> process
        Just (RPCFoundOverride ov)         -> doFoundOverride dbChan ov >> process
        Just (RPCFoundRef ru)              -> doFoundRef dbChan ru >> process
        Just (RPCFoundInclusion ic)        -> doFoundInclusion aChan dbChan dbQueryChan ic >> process
        Just (RPCLog s)                    -> logInfo s >> process
        Just RPCPing                       -> yield (encode $ RPCOK ()) >> process
        Just RPCDone                       -> return ()  -- Close connection.
        _                                  -> do liftIO $ logError "RPC server got bad request"
                                                 yield . encode $ (RPCError :: RPCResponse ())

doSendCommandInfo :: MonadIO m => IndexChan -> CommandInfo -> m ()
doSendCommandInfo aChan ci = do
  logDebug $ "RPCSendCommandInfo: " ++ (show ci)
  writeLenChan aChan $ Index (FromBuild ci)

doGetCommandInfo :: MonadIO m => DBChan -> SourceFile -> m ByteString
doGetCommandInfo dbQueryChan f = do
  logDebug $ "RPCGetCommandInfo: " ++ (show f)
  result <- callLenChan dbQueryChan $! DBGetCommandInfo f
  return $! encode $ RPCOK result

doGetSimilarCommandInfo :: MonadIO m => DBChan -> SourceFile -> m ByteString
doGetSimilarCommandInfo dbQueryChan f = do
  logDebug $ "RPCGetSimilarCommandInfo: " ++ (show f)
  result <- callLenChan dbQueryChan $! DBGetSimilarCommandInfo f
  return $! encode $ RPCOK result

doGetDefinition :: MonadIO m => DBChan -> USR -> m ByteString
doGetDefinition dbQueryChan usr = do
  logDebug $ "RPCGetDefInfo: " ++ (show usr)
  result <- callLenChan dbQueryChan $! DBGetDefinition usr
  return $! encode $ RPCOK result

doGetCallers :: MonadIO m => DBChan -> USR -> m ByteString
doGetCallers dbQueryChan usr = do
  logDebug $ "RPCGetCallers: " ++ (show usr)
  result <- callLenChan dbQueryChan $! DBGetCallers usr
  return $! encode $ RPCOK result

doGetCallees :: MonadIO m => DBChan -> USR -> m ByteString
doGetCallees dbQueryChan usr = do
  logDebug $ "RPCGetCallees: " ++ (show usr)
  result <- callLenChan dbQueryChan $! DBGetCallees usr
  return $! encode $ RPCOK result

doGetBases :: MonadIO m => DBChan -> USR -> m ByteString
doGetBases dbQueryChan usr = do
  logDebug $ "RPCGetBases: " ++ (show usr)
  result <- callLenChan dbQueryChan $! DBGetBases usr
  return $! encode $ RPCOK result

doGetOverrides :: MonadIO m => DBChan -> USR -> m ByteString
doGetOverrides dbQueryChan usr = do
  logDebug $ "RPCGetOverrides: " ++ (show usr)
  result <- callLenChan dbQueryChan $! DBGetOverrides usr
  return $! encode $ RPCOK result

doGetRefs :: MonadIO m => DBChan -> USR -> m ByteString
doGetRefs dbQueryChan usr = do
  logDebug $ "RPCGetRefs: " ++ (show usr)
  result <- callLenChan dbQueryChan $! DBGetRefs usr
  return $! encode $ RPCOK result

doGetReferenced :: MonadIO m => DBChan -> SourceLocation -> m ByteString
doGetReferenced dbQueryChan sl = do
  logDebug $ "RPCGetReferenced: " ++ (show sl)
  result <- callLenChan dbQueryChan $! DBGetReferenced sl
  return $! encode $ RPCOK result

doFoundDef :: MonadIO m => DBChan -> DefUpdate -> m ()
doFoundDef dbChan di = do
  logDebug $ "RPCFoundDef: " ++ (show di)
  writeLenChan dbChan $ DBUpdateDef di

doFoundOverride :: MonadIO m => DBChan -> Override -> m ()
doFoundOverride dbChan ov = do
  logDebug $ "RPCFoundOverride: " ++ (show ov)
  writeLenChan dbChan $ DBUpdateOverride ov

doFoundRef :: MonadIO m => DBChan -> ReferenceUpdate -> m ()
doFoundRef dbChan rf = do
  logDebug $ "RPCFoundRef: " ++ (show rf)
  writeLenChan dbChan $ DBUpdateRef rf

doFoundInclusion :: MonadIO m => IndexChan -> DBChan -> DBChan -> Inclusion -> m ()
doFoundInclusion aChan dbChan dbQueryChan ic = do
  logDebug $ "RPCFoundInclusion: " ++ (show ic)
  writeLenChan dbChan (DBUpdateInclusion ic)
  existing <- callLenChan dbQueryChan (DBInsertFileAndCheck . icHeaderFile $ ic)
  when (not existing) $
    writeLenChan aChan (Index . FromBuild . icCommandInfo $ ic)
