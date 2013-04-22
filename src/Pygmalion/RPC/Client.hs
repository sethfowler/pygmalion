{-# LANGUAGE OverloadedStrings #-}

module Pygmalion.RPC.Client
( rpcPing
, rpcIndex
, rpcGetSimilarCommandInfo
, rpcGetDefinition
, rpcGetCallers
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

rpcPing :: Port -> IO ()
rpcPing port = callRPC port RPCPing

rpcIndex :: Port -> CommandInfo -> IO ()
rpcIndex port ci = callRPC port (RPCSendCommandInfo ci)

rpcGetSimilarCommandInfo :: Port -> SourceFile -> IO (Maybe CommandInfo)
rpcGetSimilarCommandInfo port sf = callRPC port (RPCGetSimilarCommandInfo sf)

rpcGetDefinition :: Port -> USR -> IO (Maybe DefInfo)
rpcGetDefinition port usr = callRPC port (RPCGetDefinition usr)

rpcGetCallers :: Port -> USR -> IO [Invocation]
rpcGetCallers port usr = callRPC port (RPCGetCallers usr)

callRPC :: Serialize a => Port -> RPCRequest -> IO a
callRPC port req = do
    mResp <- newEmptyMVar
    runTCPClient (clientSettings port "127.0.0.1") (rpcApp req mResp)
    takeMVar mResp

rpcApp :: Serialize a => RPCRequest -> MVar a -> Application IO
rpcApp req mResp ad = ensureCompleted =<< timeout 100000000 app
  where
    app = (appSource ad) $= conduitGet getResp =$= process $$ (appSink ad)
    getResp = get
    process = do
      yield (encode req)
      result <- await
      case result of
        Just (RPCOK result') -> liftIO $ putMVar mResp $! result'
        Just RPCError        -> error "Server reported an error"
        _                    -> error "Unexpected result from server"

ensureCompleted :: Maybe a -> IO a
ensureCompleted (Just a) = return a
ensureCompleted _        = error "Connection to server timed out"
