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
        _                                  -> yield "ERROR"

doSendCommandInfo :: AnalysisChan -> CommandInfo -> IO ByteString
doSendCommandInfo aChan ci = do
  logDebug $ "RPCSendCommandInfo: " ++ (show ci)
  writeLenChan aChan $ Analyze ci
  return "OK"

doGetCommandInfo :: DBChan -> SourceFile -> IO ByteString
doGetCommandInfo dbQueryChan f = do
  logDebug $ "RPCGetCommandInfo: " ++ (show f)
  sfVar <- newEmptyMVar
  writeLenChan dbQueryChan $! DBGetCommandInfo f sfVar
  result <- takeMVar sfVar
  return $! encode result

doGetSimilarCommandInfo :: DBChan -> SourceFile -> IO ByteString
doGetSimilarCommandInfo dbQueryChan f = do
  logDebug $ "RPCGetSimilarCommandInfo: " ++ (show f)
  sfVar <- newEmptyMVar
  writeLenChan dbQueryChan $! DBGetSimilarCommandInfo f sfVar
  result <- takeMVar sfVar
  return $! encode result

doGetDefinition :: DBChan -> USR -> IO ByteString
doGetDefinition dbQueryChan usr = do
  logDebug $ "RPCGetDefInfo: " ++ (show usr)
  defVar <- newEmptyMVar
  writeLenChan dbQueryChan $! DBGetDefinition usr defVar
  result <- takeMVar defVar
  return $! encode result
