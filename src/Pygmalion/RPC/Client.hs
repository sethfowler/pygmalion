{-# LANGUAGE OverloadedStrings #-}

module Pygmalion.RPC.Client
( RPC
, RPCConnection
, openRPC
, openRPCRaw
, closeRPC
, withRPC
, withRPCRaw
, runRPC
, rpcPing
, rpcIndex
, rpcGetSimilarCommandInfo
, rpcGetDefinition
, rpcGetCallers
, rpcGetCallees
, rpcGetBases
, rpcGetOverrides
, rpcGetRefs
, rpcGetReferenced
) where

import Control.Applicative
import Control.Concurrent (newEmptyMVar, takeMVar, putMVar)
import Control.Exception (bracket)
import Control.Monad.Trans
import qualified Control.Monad.Trans.Reader as Reader
import Data.ByteString.Char8 ()
import Data.Conduit
import Data.Conduit.Cereal
import Data.Conduit.Network
import Data.Serialize
import Network.Socket
import System.Timeout

import Pygmalion.Config
import Pygmalion.Core
import Pygmalion.RPC.Request

type RPC a = Reader.ReaderT RPCConnection IO a
type RPCConnection = Socket

openRPC :: Config -> IO RPCConnection
openRPC config = fst <$> getSocket "127.0.0.1" (ifPort config)

openRPCRaw :: Port -> IO RPCConnection
openRPCRaw port = fst <$> getSocket "127.0.0.1" port

closeRPC :: RPCConnection -> IO ()
closeRPC = sClose

withRPC :: Config -> (RPCConnection -> IO a) -> IO a
withRPC config = bracket (openRPC config) closeRPC

withRPCRaw :: Port -> (RPCConnection -> IO a) -> IO a
withRPCRaw port = bracket (openRPCRaw port) closeRPC

runRPC :: RPC a -> RPCConnection -> IO a
runRPC = Reader.runReaderT

rpcPing :: RPC ()
rpcPing = callRPC RPCPing =<< Reader.ask

rpcIndex :: CommandInfo -> RPC ()
rpcIndex ci = callRPC (RPCSendCommandInfo ci) =<< Reader.ask

rpcGetSimilarCommandInfo :: SourceFile -> RPC (Maybe CommandInfo)
rpcGetSimilarCommandInfo sf = callRPC (RPCGetSimilarCommandInfo sf) =<< Reader.ask

rpcGetDefinition :: USR -> RPC (Maybe DefInfo)
rpcGetDefinition usr = callRPC (RPCGetDefinition usr) =<< Reader.ask

rpcGetCallers :: USR -> RPC [Invocation]
rpcGetCallers usr = callRPC (RPCGetCallers usr) =<< Reader.ask

rpcGetCallees :: USR -> RPC [DefInfo]
rpcGetCallees usr = callRPC (RPCGetCallees usr) =<< Reader.ask

rpcGetBases :: USR -> RPC [DefInfo]
rpcGetBases usr = callRPC (RPCGetBases usr) =<< Reader.ask

rpcGetOverrides :: USR -> RPC [DefInfo]
rpcGetOverrides usr = callRPC (RPCGetOverrides usr) =<< Reader.ask

rpcGetRefs :: USR -> RPC [SourceReference]
rpcGetRefs usr = callRPC (RPCGetRefs usr) =<< Reader.ask

rpcGetReferenced :: SourceLocation -> RPC [SourceReferenced]
rpcGetReferenced sl = callRPC (RPCGetReferenced sl) =<< Reader.ask

callRPC :: Serialize a => RPCRequest -> RPCConnection -> RPC a
callRPC req conn = liftIO $ do
    mResp <- newEmptyMVar
    ensureCompleted =<< timeout 100000000 (conduit mResp)
    takeMVar mResp
  where
    conduit mResp = sourceSocket conn
                 $= conduitGet get
                =$= process mResp
                 $$ sinkSocket conn
    process mResp = do
      yield (encode req)
      result <- await
      case result of
        Just (RPCOK result') -> liftIO $ putMVar mResp $! result'
        Just RPCError        -> error "Server reported an error"
        _                    -> error "Unexpected result from server"

ensureCompleted :: Maybe a -> IO a
ensureCompleted (Just a) = return a
ensureCompleted _        = error "Connection to server timed out"
