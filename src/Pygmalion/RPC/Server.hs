{-# LANGUAGE OverloadedStrings #-}

module Pygmalion.RPC.Server
( runRPCServer
) where

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad.Trans
import Data.ByteString.Char8 (ByteString)
import Data.Conduit
import Data.Conduit.Cereal
import Data.Conduit.Network
import Data.Serialize
import Data.String
import Network.Socket

import Pygmalion.Analysis.Manager
import Pygmalion.Config
import Pygmalion.Core
import Pygmalion.Database.Manager
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
      case result of
        Just (RPCSendCommandInfo ci) -> liftIO (doSendCommandInfo aChan ci) >>= yield
        Just (RPCGetCommandInfo sf)  -> liftIO (doGetCommandInfo dbChan sf) >>= yield
        Just (RPCGetDefinition usr)  -> liftIO (doGetDefinition dbChan usr) >>= yield
        _                            -> yield "ERROR"

doSendCommandInfo :: AnalysisChan -> CommandInfo -> IO ByteString
doSendCommandInfo aChan ci = do
  putStrLn $ "RPCSendCommandInfo: " ++ (show ci)
  writeChan aChan $ Analyze ci
  return "OK"

doGetCommandInfo :: DBChan -> SourceFile -> IO ByteString
doGetCommandInfo dbChan f = do
  putStrLn $ "RPCGetCommandInfo: " ++ (show f)
  sfVar <- newEmptyMVar
  writeChan dbChan $! DBGetCommandInfo f sfVar
  result <- takeMVar sfVar
  return $! encode result

doGetDefinition :: DBChan -> USR -> IO ByteString
doGetDefinition dbChan usr = do
  putStrLn $ "RPCGetDefInfo: " ++ (show usr)
  defVar <- newEmptyMVar
  writeChan dbChan $! DBGetDefinition usr defVar
  result <- takeMVar defVar
  return $! encode result
