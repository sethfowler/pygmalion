{-# LANGUAGE OverloadedStrings #-}

module Pygmalion.RPC.Server
( runRPCServer
) where

import Control.Monad
import Control.Monad.Trans
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
        Just (RPCSendCommandInfo ci)       -> liftIO (doSendCommandInfo aChan ci) >> process
        Just (RPCGetCommandInfo sf)        -> liftIO (doGetCommandInfo dbQueryChan sf) >>= yield >> process
        Just (RPCGetSimilarCommandInfo sf) -> liftIO (doGetSimilarCommandInfo dbQueryChan sf) >>= yield >> process
        Just (RPCGetDefinition usr)        -> liftIO (doGetDefinition dbQueryChan usr) >>= yield >> process
        Just (RPCGetCallers usr)           -> liftIO (doGetCallers dbQueryChan usr) >>= yield >> process
        Just (RPCGetCallees usr)           -> liftIO (doGetCallees dbQueryChan usr) >>= yield >> process
        Just (RPCGetBases usr)             -> liftIO (doGetBases dbQueryChan usr) >>= yield >> process
        Just (RPCGetOverrides usr)         -> liftIO (doGetOverrides dbQueryChan usr) >>= yield >> process
        Just (RPCGetRefs usr)              -> liftIO (doGetRefs dbQueryChan usr) >>= yield >> process
        Just (RPCGetReferenced sl)         -> liftIO (doGetReferenced dbQueryChan sl) >>= yield >> process
        Just (RPCFoundDef du)              -> liftIO (doFoundDef dbChan du) >> process
        Just (RPCFoundOverride ov)         -> liftIO (doFoundOverride dbChan ov) >> process
        Just (RPCFoundRef ru)              -> liftIO (doFoundRef dbChan ru) >> process
        Just (RPCFoundInclusion ic)        -> liftIO (doFoundInclusion aChan dbChan dbQueryChan ic) >> process
        Just (RPCLog s)                    -> liftIO (logInfo s) >> process
        Just RPCPing                       -> yield (encode $ RPCOK ()) >> process
        Just RPCDone                       -> return ()  -- Close connection.
        _                                  -> do liftIO $ logError "RPC server got bad request"
                                                 yield . encode $ (RPCError :: RPCResponse ())

doSendCommandInfo :: IndexChan -> CommandInfo -> IO ()
doSendCommandInfo aChan ci = do
  logDebug $ "RPCSendCommandInfo: " ++ (show ci)
  writeLenChan aChan $ Index (FromBuild ci)

doGetCommandInfo :: DBChan -> SourceFile -> IO ByteString
doGetCommandInfo dbQueryChan f = do
  logDebug $ "RPCGetCommandInfo: " ++ (show f)
  result <- callLenChan dbQueryChan $! DBGetCommandInfo f
  return $! encode $ RPCOK result

doGetSimilarCommandInfo :: DBChan -> SourceFile -> IO ByteString
doGetSimilarCommandInfo dbQueryChan f = do
  logDebug $ "RPCGetSimilarCommandInfo: " ++ (show f)
  result <- callLenChan dbQueryChan $! DBGetSimilarCommandInfo f
  return $! encode $ RPCOK result

doGetDefinition :: DBChan -> USR -> IO ByteString
doGetDefinition dbQueryChan usr = do
  logDebug $ "RPCGetDefInfo: " ++ (show usr)
  result <- callLenChan dbQueryChan $! DBGetDefinition usr
  return $! encode $ RPCOK result

doGetCallers :: DBChan -> USR -> IO ByteString
doGetCallers dbQueryChan usr = do
  logDebug $ "RPCGetCallers: " ++ (show usr)
  result <- callLenChan dbQueryChan $! DBGetCallers usr
  mapM_ print result
  return $! encode $ RPCOK result

doGetCallees :: DBChan -> USR -> IO ByteString
doGetCallees dbQueryChan usr = do
  logDebug $ "RPCGetCallees: " ++ (show usr)
  result <- callLenChan dbQueryChan $! DBGetCallees usr
  mapM_ print result
  return $! encode $ RPCOK result

doGetBases :: DBChan -> USR -> IO ByteString
doGetBases dbQueryChan usr = do
  logDebug $ "RPCGetBases: " ++ (show usr)
  result <- callLenChan dbQueryChan $! DBGetBases usr
  mapM_ print result
  return $! encode $ RPCOK result

doGetOverrides :: DBChan -> USR -> IO ByteString
doGetOverrides dbQueryChan usr = do
  logDebug $ "RPCGetOverrides: " ++ (show usr)
  result <- callLenChan dbQueryChan $! DBGetOverrides usr
  mapM_ print result
  return $! encode $ RPCOK result

doGetRefs :: DBChan -> USR -> IO ByteString
doGetRefs dbQueryChan usr = do
  logDebug $ "RPCGetRefs: " ++ (show usr)
  result <- callLenChan dbQueryChan $! DBGetRefs usr
  mapM_ print result
  return $! encode $ RPCOK result

doGetReferenced :: DBChan -> SourceLocation -> IO ByteString
doGetReferenced dbQueryChan sl = do
  logDebug $ "RPCGetReferenced: " ++ (show sl)
  result <- callLenChan dbQueryChan $! DBGetReferenced sl
  mapM_ print result
  return $! encode $ RPCOK result

doFoundDef :: DBChan -> DefUpdate -> IO ()
doFoundDef dbChan di = do
  logDebug $ "RPCFoundDef: " ++ (show di)
  writeLenChan dbChan $ DBUpdateDef di

doFoundOverride :: DBChan -> Override -> IO ()
doFoundOverride dbChan ov = do
  logDebug $ "RPCFoundOverride: " ++ (show ov)
  writeLenChan dbChan $ DBUpdateOverride ov

doFoundRef :: DBChan -> ReferenceUpdate -> IO ()
doFoundRef dbChan rf = do
  logDebug $ "RPCFoundRef: " ++ (show rf)
  writeLenChan dbChan $ DBUpdateRef rf

doFoundInclusion :: IndexChan -> DBChan -> DBChan -> Inclusion -> IO ()
doFoundInclusion aChan dbChan dbQueryChan ic = do
  logDebug $ "RPCFoundInclusion: " ++ (show ic)
  writeLenChan dbChan (DBUpdateInclusion ic)
  existing <- callLenChan dbQueryChan (DBInsertFileAndCheck . icHeaderFile $ ic)
  when (not existing) $
    writeLenChan aChan (Index . FromBuild . icCommandInfo $ ic)
