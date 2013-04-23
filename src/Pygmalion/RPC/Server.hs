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

runRPCServer :: Config -> MVar Int -> AnalysisChan -> DBChan -> IO ()
runRPCServer cf port aChan dbQueryChan =
    runTCPServer settings (serverApp aChan dbQueryChan)
  where
    settings = baseSettings { serverAfterBind = notifyPort }
    baseSettings = (serverSettings confPort confAddr) :: ServerSettings IO
    confPort = ifPort cf
    confAddr = fromString (ifAddr cf)
    notifyPort s = socketPort s >>= (putMVar port) . fromIntegral

serverApp :: AnalysisChan -> DBChan -> Application IO
serverApp aChan dbQueryChan ad =
    (appSource ad) $= conduitGet getCI =$= process $$ (appSink ad)
  where
    getCI = {-# SCC "serverget" #-} get :: Get RPCRequest
    process = do
      result <- await
      case result of
        Just (RPCSendCommandInfo ci)       -> liftIO (doSendCommandInfo aChan ci) >>= yield
        Just (RPCGetCommandInfo sf)        -> liftIO (doGetCommandInfo dbQueryChan sf) >>= yield
        Just (RPCGetSimilarCommandInfo sf) -> liftIO (doGetSimilarCommandInfo dbQueryChan sf) >>= yield
        Just (RPCGetDefinition usr)        -> liftIO (doGetDefinition dbQueryChan usr) >>= yield
        Just (RPCGetCallers usr)           -> liftIO (doGetCallers dbQueryChan usr) >>= yield
        Just (RPCGetCallees usr)           -> liftIO (doGetCallees dbQueryChan usr) >>= yield
        Just (RPCGetRefs usr)              -> liftIO (doGetRefs dbQueryChan usr) >>= yield
        Just RPCPing                       -> yield . encode $ RPCOK ()
        _                                  -> yield . encode $ (RPCError :: RPCResponse ())

doSendCommandInfo :: AnalysisChan -> CommandInfo -> IO ByteString
doSendCommandInfo aChan ci = do
  logDebug $ "RPCSendCommandInfo: " ++ (show ci)
  writeLenChan aChan $ Analyze ci
  return $! encode $ RPCOK ()

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

doGetRefs :: DBChan -> USR -> IO ByteString
doGetRefs dbQueryChan usr = do
  logDebug $ "RPCGetRefs: " ++ (show usr)
  result <- callLenChan dbQueryChan $! DBGetRefs usr
  mapM_ print result
  return $! encode $ RPCOK result
