{-# LANGUAGE OverloadedStrings #-}

module Pygmalion.RPC.Client
( sendPing
, sendScanMessage
, lookupSimilarCommandInfo
, lookupDefInfo
, lookupCallers
) where

import Control.Concurrent (newEmptyMVar, takeMVar, putMVar, MVar)
import Control.Monad.Trans
import Data.ByteString.Char8 ()
import Data.Conduit
import Data.Conduit.Cereal
import Data.Conduit.Network
import Data.Serialize
import System.Timeout

import Pygmalion.Core
import Pygmalion.RPC.Request

sendPing :: Port -> IO ()
sendPing port = runTCPClient settings pingApp
  where settings = clientSettings port "127.0.0.1"

pingApp :: Application IO
pingApp ad = ensureCompleted =<< timeout hundredSeconds app
  where 
    hundredSeconds = 100000000
    app = (appSource ad) $= conduitGet getResp =$= process $$ (appSink ad)
    getResp = get :: Get (RPCResponse ())
    process = do
      yield (encode RPCPing)
      result <- await
      case result of
        Just (RPCOK _) -> return ()
        Just RPCError  -> error "Server reported an error"
        _              -> error "Unexpected result from server"

sendScanMessage :: Port -> CommandInfo -> IO ()
sendScanMessage port ci = runTCPClient settings (scanApp ci)
  where settings = clientSettings port "127.0.0.1"

scanApp :: CommandInfo -> Application IO
scanApp ci ad = ensureCompleted =<< timeout hundredSeconds app
  where 
    hundredSeconds = 100000000
    app = (appSource ad) $= conduitGet getResp =$= process $$ (appSink ad)
    getResp = get :: Get (RPCResponse ())
    process = do
      yield (encode $ RPCSendCommandInfo ci)
      result <- await
      case result of
        Just (RPCOK _) -> return ()
        Just RPCError  -> error "Server reported an error"
        _              -> error "Unexpected result from server"

-- FIXME: This can be made much more efficient and cleaned up.
lookupSimilarCommandInfo :: Port -> SourceFile -> IO (Maybe CommandInfo)
lookupSimilarCommandInfo port sf = do
    result <- newEmptyMVar
    runTCPClient settings (lookupSimilarCIApp sf result)
    takeMVar result
  where settings = clientSettings port "127.0.0.1"

lookupSimilarCIApp :: SourceFile -> MVar (Maybe CommandInfo) -> Application IO
lookupSimilarCIApp sf v ad = ensureCompleted =<< timeout hundredSeconds app
  where 
    hundredSeconds = 100000000
    app = (appSource ad) $= conduitGet getCI =$= process $$ (appSink ad)
    getCI = get :: Get (RPCResponse (Maybe CommandInfo))
    process = do
      yield (encode $ RPCGetSimilarCommandInfo sf)
      result <- await
      case result of
        Just (RPCOK mayCI) -> liftIO $ putMVar v $! mayCI
        _                  -> error "Unexpected result from server"

lookupDefInfo :: Port -> USR -> IO (Maybe DefInfo)
lookupDefInfo port usr = do
    result <- newEmptyMVar
    runTCPClient settings (lookupDefApp usr result)
    takeMVar result
  where settings = clientSettings port "127.0.0.1"

lookupDefApp :: USR -> MVar (Maybe DefInfo) -> Application IO
lookupDefApp usr v ad = ensureCompleted =<< timeout hundredSeconds app
  where 
    hundredSeconds = 100000000
    app = (appSource ad) $= conduitGet getDI =$= process $$ (appSink ad)
    getDI = get :: Get (RPCResponse (Maybe DefInfo))
    process = do
      yield (encode $ RPCGetDefinition usr)
      result <- await
      case result of
        Just (RPCOK mayDef) -> liftIO $ putMVar v $! mayDef
        _                   -> error "Unexpected result from server"

lookupCallers :: Port -> USR -> IO [DefInfo]
lookupCallers port usr = do
    result <- newEmptyMVar
    runTCPClient settings (lookupCallerApp usr result)
    takeMVar result
  where settings = clientSettings port "127.0.0.1"

lookupCallerApp :: USR -> MVar [DefInfo] -> Application IO
lookupCallerApp usr v ad = ensureCompleted =<< timeout hundredSeconds app
  where 
    hundredSeconds = 100000000
    app = (appSource ad) $= conduitGet getDI =$= process $$ (appSink ad)
    getDI = get :: Get (RPCResponse [DefInfo])
    process = do
      yield (encode $ RPCGetCallers usr)
      result <- await
      case result of
        Just (RPCOK callers) -> liftIO $ putMVar v $! callers
        _                   -> error "Unexpected result from server"

ensureCompleted :: Maybe a -> IO a
ensureCompleted (Just a) = return a
ensureCompleted _        = error "Connection to server timed out"
