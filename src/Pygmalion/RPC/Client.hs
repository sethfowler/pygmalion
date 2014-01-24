{-# LANGUAGE DeriveDataTypeable, ForeignFunctionInterface, OverloadedStrings #-}

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
, rpcWait
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
, rpcGetMembers
, rpcGetRefs
, rpcGetReferenced
, rpcGetDeclReferenced
, rpcGetHierarchy
, rpcGetInclusions
, rpcGetIncluders
, rpcGetInclusionHierarchy
, rpcSendUpdates
, rpcUpdateAndFindDirtyInclusions
) where

import Control.Concurrent (newEmptyMVar, takeMVar, putMVar)
import Control.Exception
import Control.Monad.Trans
import qualified Control.Monad.Trans.Reader as Reader
import Data.ByteString.Char8 ()
import Data.Conduit
import Data.Conduit.Cereal
import Data.Conduit.Network.Unix
import Data.Serialize
import Data.Typeable
import qualified Data.Vector as V
import Network.Socket
import System.IO.Error
import System.Posix.Process (getProcessID)
import System.Timeout

import Pygmalion.Config
import Pygmalion.Core
import Pygmalion.Database.Request
import Pygmalion.RPC.Request

type RPC a = Reader.ReaderT RPCConnection IO a
type RPCConnection = Socket

openRPC :: Config -> IO RPCConnection
openRPC config = getSocket (socketPath config)

openRPCRaw :: FilePath -> IO RPCConnection
openRPCRaw path = getSocket path

closeRPC :: RPCConnection -> IO ()
closeRPC conn = do
  -- Notify the server that nothing else is coming.
  runRPC rpcDone conn

  -- sClose (really the underlying 'close' system call) can sometimes block,
  -- locking up the thread permanently. Once GHC 7.8 arrives with
  -- 'interruptible' foreign imports, we'll be able to use timeout,
  -- but until that time the safest approach appears to be to shutdown
  -- but not close the socket. This leaks a file descriptor, but
  -- that's OK, since pygmalion currently only opens one RPC
  -- connection per process.
  --sClose conn
  ensureSocketShutdown =<< try (shutdown conn ShutdownBoth)

withRPC :: Config -> (RPCConnection -> IO a) -> IO a
withRPC config = bracket (openRPC config) closeRPC

withRPCRaw :: FilePath -> (RPCConnection -> IO a) -> IO a
withRPCRaw path = bracket (openRPCRaw path) closeRPC

runRPC :: RPC a -> RPCConnection -> IO a
runRPC = Reader.runReaderT

rpcDone :: RPC ()
rpcDone = callRPCInfallible_ RPCDone =<< Reader.ask

rpcStop :: RPC ()
rpcStop = callRPCInfallible_ RPCStop =<< Reader.ask

rpcWait :: RPC ()
rpcWait = callRPCWaitForever RPCWait =<< Reader.ask

rpcPing :: RPC ()
rpcPing = callRPC RPCPing =<< Reader.ask

rpcLog :: String -> RPC ()
rpcLog s = callRPC_ (RPCLog s) =<< Reader.ask

rpcIndexCommand :: CommandInfo -> Time -> RPC ()
rpcIndexCommand ci mtime = callRPC_ (RPCIndexCommand ci mtime) =<< Reader.ask

rpcIndexFile :: SourceFile -> Time -> RPC ()
rpcIndexFile sf mtime = callRPC_ (RPCIndexFile sf mtime) =<< Reader.ask

rpcGetSimilarCommandInfo :: SourceFile -> RPC (Maybe CommandInfo)
rpcGetSimilarCommandInfo sf = callRPC (RPCGetSimilarCommandInfo sf) =<< Reader.ask

rpcGetDefinition :: SourceLocation -> RPC [DefInfo]
rpcGetDefinition sl = callRPC (RPCGetDefinition sl) =<< Reader.ask

rpcGetCallers :: SourceLocation -> RPC [Invocation]
rpcGetCallers sl = callRPC (RPCGetCallers sl) =<< Reader.ask

rpcGetCallees :: SourceLocation -> RPC [DefInfo]
rpcGetCallees sl = callRPC (RPCGetCallees sl) =<< Reader.ask

rpcGetBases :: SourceLocation -> RPC [DefInfo]
rpcGetBases sl = callRPC (RPCGetBases sl) =<< Reader.ask

rpcGetOverrides :: SourceLocation -> RPC [DefInfo]
rpcGetOverrides sl = callRPC (RPCGetOverrides sl) =<< Reader.ask

rpcGetMembers :: SourceLocation -> RPC [DefInfo]
rpcGetMembers sl = callRPC (RPCGetMembers sl) =<< Reader.ask

rpcGetRefs :: SourceLocation -> RPC [SourceReference]
rpcGetRefs sl = callRPC (RPCGetRefs sl) =<< Reader.ask

rpcGetReferenced :: SourceLocation -> RPC (Maybe SourceReferenced)
rpcGetReferenced sl = callRPC (RPCGetReferenced sl) =<< Reader.ask

rpcGetDeclReferenced :: SourceLocation -> RPC [DefInfo]
rpcGetDeclReferenced sl = callRPC (RPCGetDeclReferenced sl) =<< Reader.ask

rpcGetHierarchy :: SourceLocation -> RPC String
rpcGetHierarchy sl = callRPC (RPCGetHierarchy sl) =<< Reader.ask

rpcGetInclusions :: SourceFile -> RPC [SourceFile]
rpcGetInclusions sf = callRPC (RPCGetInclusions sf) =<< Reader.ask

rpcGetIncluders :: SourceFile -> RPC [SourceFile]
rpcGetIncluders sf = callRPC (RPCGetIncluders sf) =<< Reader.ask

rpcGetInclusionHierarchy :: SourceFile -> RPC String
rpcGetInclusionHierarchy sf = callRPC (RPCGetInclusionHierarchy sf) =<< Reader.ask

rpcSendUpdates :: V.Vector DBUpdate -> RPC ()
rpcSendUpdates ups = callRPC_ (RPCFoundUpdates ups) =<< Reader.ask

rpcUpdateAndFindDirtyInclusions :: SourceFileHash -> [Inclusion] -> RPC [SourceFileHash]
rpcUpdateAndFindDirtyInclusions sfHash ics =
  callRPC (RPCUpdateAndFindDirtyInclusions sfHash ics) =<< Reader.ask

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
callRPC_ req conn = liftIO timedConduit
  where
    timedConduit = ensureCompleted =<< timeout 100000000 conduit
    conduit = process $$ sinkSocket conn
    process = yield (encode req)

callRPCInfallible_ :: RPCRequest -> RPCConnection -> RPC ()
callRPCInfallible_ req conn = liftIO $ catchIOError timedConduit (const $ return ())
  where
    timedConduit = ensureCompleted =<< timeout 100000000 conduit
    conduit = process $$ sinkSocket conn
    process = yield (encode req)

callRPCWaitForever :: Serialize a => RPCRequest -> RPCConnection -> RPC a
callRPCWaitForever req conn = liftIO $ do
    mResp <- newEmptyMVar
    conduit mResp
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

ensureCompleted :: Maybe a -> IO a
ensureCompleted (Just a) = return a
ensureCompleted _        = throw $ RPCException "Connection to server timed out"

ensureSocketShutdown :: Either SomeException () -> IO ()
ensureSocketShutdown (Left e) = do pid <- getProcessID
                                   putStrLn $ show pid ++ ": Exception on RPC socket shutdown: "
                                           ++ show (e :: SomeException) ++ ")"
ensureSocketShutdown _        = return ()

data RPCException = RPCException String
  deriving (Show, Typeable)
instance Exception RPCException
