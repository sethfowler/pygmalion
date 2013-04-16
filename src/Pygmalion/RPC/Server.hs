{-# LANGUAGE OverloadedStrings #-}

module Pygmalion.RPC.Server
( runRPCServer
) where

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad.Trans
import Data.ByteString.Char8
import Data.Conduit
import Data.Conduit.Cereal
import Data.Conduit.Network
import Data.Serialize
import Data.String
import Network.Socket

import Pygmalion.Analyze
import Pygmalion.Config
import Pygmalion.Core
import Pygmalion.RPC.Request

runRPCServer :: Config -> MVar Int -> AnalysisChan -> DBChan -> IO ()
runRPCServer cf port aChan dbChan = runTCPServer settings (serverApp aChan dbChan)
  where settings = baseSettings { serverAfterBind = notifyPort }
        baseSettings = (serverSettings confPort confAddr) :: ServerSettings IO
        confPort = ifPort cf
        confAddr = fromString (ifAddr cf)
        notifyPort s = socketPort s >>= (putMVar port) . fromIntegral

serverApp :: AnalysisChan -> DBChan -> Application IO
serverApp aChan dbChan ad =
    (appSource ad) $= conduitGet getCI =$= process $$ (appSink ad)
  where
    getCI = {-# SCC "serverget" #-} get :: Get RPCRequest
    process = do
      result <- await
      -- liftIO $ putStrLn $ "Server got: " ++ (show result)
      case result of
        Just (RPCSendCommandInfo ci) -> liftIO (doSendCommandInfo aChan ci) >>= yield
        Just (RPCGetCommandInfo sf)  -> liftIO (doGetCommandInfo dbChan sf) >>= yield
        Just (RPCGetDefinition usr)  -> liftIO (doGetDefinition dbChan usr) >>= yield
        _                            -> yield "ERROR"

doSendCommandInfo :: AnalysisChan -> CommandInfo -> IO ByteString
doSendCommandInfo aChan ci = do
  writeChan aChan $ Analyze ci
  return "OK"

doGetCommandInfo :: DBChan -> SourceFile -> IO ByteString
doGetCommandInfo dbChan f = do
  sfVar <- newEmptyMVar
  writeChan dbChan $! DBGetCommandInfo f sfVar
  result <- readMVar sfVar
  return $! encode result

doGetDefinition :: DBChan -> USR -> IO ByteString
doGetDefinition dbChan usr = do
  defVar <- newEmptyMVar
  writeChan dbChan $! DBGetDefinition usr defVar
  result <- readMVar defVar
  return $! encode result
