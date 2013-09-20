{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

module Pygmalion.RPC.Client
( RPC
, RPCConnection
, openRPC
, openRPCRaw
, closeRPC
, withRPC
, withRPCRaw
, runRPC
, rpcStop
, rpcPing
, rpcLog
, rpcIndexCommand
, rpcIndexFile
, rpcGetSimilarCommandInfo
, rpcGetDefinition
, rpcGetCallers
, rpcGetCallees
, rpcGetBases
, rpcGetOverrides
, rpcGetRefs
, rpcGetReferenced
, rpcFoundDef
, rpcFoundOverride
, rpcFoundRef
, rpcFoundInclusion
) where

import Control.Applicative
import Control.Concurrent (newEmptyMVar, takeMVar, putMVar)
import Control.Exception
import Control.Monad.Trans
import qualified Control.Monad.Trans.Reader as Reader
import Data.ByteString.Char8 ()
import Data.Conduit
import Data.Conduit.Cereal
import Data.Conduit.Network
import Data.Serialize
import Data.Typeable
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
closeRPC conn = do
  runRPC rpcDone conn
  sClose conn

withRPC :: Config -> (RPCConnection -> IO a) -> IO a
withRPC config = bracket (openRPC config) closeRPC

withRPCRaw :: Port -> (RPCConnection -> IO a) -> IO a
withRPCRaw port = bracket (openRPCRaw port) closeRPC

runRPC :: RPC a -> RPCConnection -> IO a
runRPC = Reader.runReaderT

rpcDone :: RPC ()
rpcDone = callRPC_ RPCDone =<< Reader.ask

rpcStop :: RPC ()
rpcStop = callRPC_ RPCStop =<< Reader.ask

rpcPing :: RPC ()
rpcPing = callRPC RPCPing =<< Reader.ask

rpcLog :: String -> RPC ()
rpcLog s = callRPC_ (RPCLog s) =<< Reader.ask

rpcIndexCommand :: CommandInfo -> RPC ()
rpcIndexCommand ci = callRPC_ (RPCIndexCommand ci) =<< Reader.ask

rpcIndexFile :: SourceFile -> RPC ()
rpcIndexFile sf = callRPC_ (RPCIndexFile sf) =<< Reader.ask

rpcGetSimilarCommandInfo :: SourceFile -> RPC (Maybe CommandInfo)
rpcGetSimilarCommandInfo sf = callRPC (RPCGetSimilarCommandInfo sf) =<< Reader.ask

rpcGetDefinition :: SourceLocation -> RPC [DefInfo]
rpcGetDefinition sl = callRPC (RPCGetDefinition sl) =<< Reader.ask

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

rpcGetReferenced :: SourceLocation -> RPC (Maybe SourceReferenced)
rpcGetReferenced sl = callRPC (RPCGetReferenced sl) =<< Reader.ask

rpcFoundDef :: DefUpdate -> RPC ()
rpcFoundDef di = callRPC_ (RPCFoundDef di) =<< Reader.ask

rpcFoundOverride :: Override -> RPC ()
rpcFoundOverride ov = callRPC_ (RPCFoundOverride ov) =<< Reader.ask

rpcFoundRef :: ReferenceUpdate -> RPC ()
rpcFoundRef rf = callRPC_ (RPCFoundRef rf) =<< Reader.ask

rpcFoundInclusion :: Inclusion -> RPC ()
rpcFoundInclusion ic = callRPC_ (RPCFoundInclusion ic) =<< Reader.ask

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
        Just RPCError        -> throw $ RPCException "Server reported an error"
        _                    -> throw $ RPCException "Unexpected result from server"

callRPC_ :: RPCRequest -> RPCConnection -> RPC ()
callRPC_ req conn = liftIO $ ensureCompleted =<< timeout 100000000 conduit 
  where
    conduit = process $$ sinkSocket conn
    process = yield (encode req)

ensureCompleted :: Maybe a -> IO a
ensureCompleted (Just a) = return a
ensureCompleted _        = throw $ RPCException "Connection to server timed out"

data RPCException = RPCException String
  deriving (Show, Typeable)
instance Exception RPCException
