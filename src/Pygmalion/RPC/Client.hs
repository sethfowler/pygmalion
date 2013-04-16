{-# LANGUAGE OverloadedStrings #-}

module Pygmalion.RPC.Client
( sendScanMessage
, lookupCommandInfo
, lookupDefInfo
) where

import Control.Concurrent (newEmptyMVar, readMVar, putMVar, MVar)
import Control.Monad.Trans
import Data.ByteString.Char8 ()
import Data.Conduit
import Data.Conduit.Cereal
import Data.Conduit.Network
import Data.Serialize
import System.Timeout

import Pygmalion.Core
import Pygmalion.RPC.Request

sendScanMessage :: Port -> CommandInfo -> IO ()
sendScanMessage port ci = runTCPClient settings (scanApp ci)
  where settings = clientSettings port "127.0.0.1"

scanApp :: CommandInfo -> Application IO
scanApp ci ad = ensureCompleted =<< timeout hundredSeconds app
  where 
    hundredSeconds = 100000000
    app = (appSource ad) $$ conduit =$ (appSink ad)
    conduit = do
      yield (encode $ RPCSendCommandInfo ci)
      result <- await
      case result of
        Just "OK"    -> return ()
        Just "ERROR" -> error "Server reported an error"
        _            -> error "Unexpected result from server"

-- FIXME: This can be made much more efficient and cleaned up.
lookupCommandInfo :: Port -> SourceFile -> IO (Maybe CommandInfo)
lookupCommandInfo port sf = do
    result <- newEmptyMVar
    runTCPClient settings (lookupCIApp sf result)
    readMVar result
  where settings = clientSettings port "127.0.0.1"

lookupCIApp :: SourceFile -> MVar (Maybe CommandInfo) -> Application IO
lookupCIApp sf v ad = ensureCompleted =<< timeout hundredSeconds app
  where 
    hundredSeconds = 100000000
    app = (appSource ad) $= conduitGet getCI =$= process $$ (appSink ad)
    getCI = get :: Get (Maybe CommandInfo)
    process = do
      yield (encode $ RPCGetCommandInfo sf)
      result <- await
      case result of
        Just mayCI -> liftIO $ putMVar v $! mayCI
        _          -> error "Unexpected result from server"

lookupDefInfo :: Port -> USR -> IO (Maybe DefInfo)
lookupDefInfo port usr = do
    result <- newEmptyMVar
    runTCPClient settings (lookupDefApp usr result)
    readMVar result
  where settings = clientSettings port "127.0.0.1"

lookupDefApp :: USR -> MVar (Maybe DefInfo) -> Application IO
lookupDefApp usr v ad = ensureCompleted =<< timeout hundredSeconds app
  where 
    hundredSeconds = 100000000
    app = (appSource ad) $= conduitGet getDI =$= process $$ (appSink ad)
    getDI = get :: Get (Maybe DefInfo)
    process = do
      yield (encode $ RPCGetDefinition usr)
      result <- await
      case result of
        Just mayDef -> liftIO $ putMVar v $! mayDef
        _           -> error "Unexpected result from server"

ensureCompleted :: Maybe a -> IO a
ensureCompleted (Just a) = return a
ensureCompleted _        = error "Connection to server timed out"
