{-# LANGUAGE OverloadedStrings #-}

module Pygmalion.RPC.Server
( runRPCServer
) where

-- import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad.Trans
import Data.ByteString.Char8 (ByteString)
import Data.Conduit
import Data.Conduit.Cereal
import Data.Conduit.Network
import Data.Serialize
import Data.String
import Network.Socket

import Control.Concurrent.Chan.Len
import Pygmalion.Analysis.Manager
import Pygmalion.Config
import Pygmalion.Core
import Pygmalion.Database.Manager
import Pygmalion.Log
import Pygmalion.RPC.Request

runRPCServer :: Config -> MVar Int -> AnalysisChan -> DBChan -> DBChan -> IO ()
runRPCServer cf port aChan dbChan dbQueryChan =
    runTCPServer settings (serverApp aChan dbChan dbQueryChan)
  where
    settings = baseSettings { serverAfterBind = notifyPort }
    baseSettings = (serverSettings confPort confAddr) :: ServerSettings IO
    confPort = ifPort cf
    confAddr = fromString (ifAddr cf)
    notifyPort s = socketPort s >>= (putMVar port) . fromIntegral

serverApp :: AnalysisChan -> DBChan -> DBChan -> Application IO
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
        Just (RPCFoundDef di)              -> liftIO (doFoundDef dbChan di) >> process
        Just (RPCFoundOverride ov)         -> liftIO (doFoundOverride dbChan ov) >> process
        Just (RPCFoundRef rf)              -> liftIO (doFoundRef dbChan rf) >> process
        Just (RPCFoundInclusion ic)        -> liftIO (doFoundInclusion aChan dbChan ic) >> process
        Just (RPCLog s)                    -> liftIO (logInfo s) >> process
        Just RPCPing                       -> yield (encode $ RPCOK ()) >> process
        Just RPCDone                       -> return ()  -- Close connection.
        _                                  -> do liftIO $ logError "RPC server got bad request"
                                                 yield . encode $ (RPCError :: RPCResponse ())

doSendCommandInfo :: AnalysisChan -> CommandInfo -> IO ()
doSendCommandInfo aChan ci = do
  logDebug $ "RPCSendCommandInfo: " ++ (show ci)
  writeLenChan aChan $ AnalyzeBuiltFile ci

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

doFoundDef :: DBChan -> DefInfo -> IO ()
doFoundDef dbChan di = do
  logDebug $ "RPCFoundDef: " ++ (show di)
  writeLenChan dbChan $ DBUpdateDefInfo di

doFoundOverride :: DBChan -> Override -> IO ()
doFoundOverride dbChan ov = do
  logDebug $ "RPCFoundOverride: " ++ (show ov)
  writeLenChan dbChan $ DBUpdateOverride ov

doFoundRef :: DBChan -> Reference -> IO ()
doFoundRef dbChan rf = do
  logDebug $ "RPCFoundRef: " ++ (show rf)
  writeLenChan dbChan $ DBUpdateRef rf

doFoundInclusion :: AnalysisChan -> DBChan -> Inclusion -> IO ()
doFoundInclusion aChan dbChan ic = do
  logDebug $ "RPCFoundInclusion: " ++ (show ic)
  liftIO (writeLenChan dbChan (DBUpdateInclusion ic))
  liftIO (writeLenChan aChan (AnalyzeBuiltFile . icCommandInfo $ ic))
