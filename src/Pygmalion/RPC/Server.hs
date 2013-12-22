{-# LANGUAGE OverloadedStrings, BangPatterns, DeriveDataTypeable #-}

module Pygmalion.RPC.Server
( runRPCServer
, RPCServerExit(..)
) where

import Control.Concurrent (ThreadId, myThreadId)
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.ByteString.Char8 (ByteString)
import Data.Conduit
import Data.Conduit.Cereal
import Data.Conduit.Network.Unix
import Data.Serialize
import Data.Typeable
import qualified Data.Vector as VV

import Control.Concurrent.MVar
import Control.Concurrent.Chan.Len
import Pygmalion.Config
import Pygmalion.Core
import Pygmalion.Database.Request
import Pygmalion.Idle
import Pygmalion.Index.Manager
import Pygmalion.Index.Request
import Pygmalion.Index.Stream
import Pygmalion.Log
import Pygmalion.RPC.Request

runRPCServer :: Config -> IndexStream -> DBUpdateChan -> DBQueryChan -> IdleChan -> IO ()
runRPCServer cf iStream dbUpdateChan dbQueryChan idleChan = do
    conns <- newMVar 0
    threadId <- myThreadId
    runUnixServer settings (serverApp $ RPCServerContext threadId
                                                         iStream
                                                         dbUpdateChan
                                                         dbQueryChan
                                                         idleChan
                                                         conns)
  where
    settings = serverSettings path :: ServerSettings IO
    path = socketPath cf

serverApp :: RPCServerContext -> Application IO
serverApp ctx ad = do
    open
    conduit `onException` closeException
  where
    getCI = get :: Get RPCRequest

    conduit = appSource ad $= conduitGet getCI =$= process $$ appSink ad

    open = liftIO $ modifyMVar_ (rsConnections ctx) (\c -> return (c + 1))
    close = liftIO $ terminate' 1
    closeException = terminate' 1
    terminate = liftIO $ terminate' 2

    terminate' v = do
      -- If v == 1, this balances the increment to rsConnections that
      -- happened when we opened this connection. If v > 1, this
      -- ensures that rsConnections will eventually drop below 0,
      -- terminating the server.
      conns <- takeMVar (rsConnections ctx)
      let newConns = conns - v
      putMVar (rsConnections ctx) newConns
      when (newConns < 0) $
        throwTo (rsThread ctx) RPCServerExit -- Terminate the server.

    process :: ConduitM RPCRequest ByteString IO ()
    process = do
        result <- await
        case result of
          Just RPCDone                -> close
          Just RPCStop                -> terminate
          Just (RPCFoundUpdates !ups) -> do liftIO $ sendUpdates ctx ups
                                            process
          Just req                    -> do res <- liftIO $ runReaderT (route req) ctx
                                            case res of
                                               Just res' -> yield res' >> process
                                               Nothing   -> process
          _                           -> do logError "RPC server got bad request"
                                            yield . encode $ (RPCError :: RPCResponse ())
                                            close

{-
sendUpdates :: RPCServerContext -> V.IOVector DBUpdate -> Int -> IO ()
sendUpdates _ _ 0         = return ()
sendUpdates ctx !ups !upn = do
  v <- VV.unsafeFreeze $ V.unsafeSlice 0 upn ups
  writeLenChan (rsDBUpdateChan ctx) v
-}

sendUpdates :: RPCServerContext -> VV.Vector DBUpdate -> IO ()
--sendUpdates ctx !ups = pushUpdate (rsDBUpdateChan ctx) ups
sendUpdates _ _ = return ()

data RPCServerContext = RPCServerContext
  { rsThread       :: !ThreadId
  , rsIndexStream  :: !IndexStream
  , rsDBUpdateChan :: !DBUpdateChan
  , rsDBQueryChan  :: !DBQueryChan
  , rsIdleChan     :: !IdleChan
  , rsConnections  :: !(MVar Int)
  }
type RPCServer a = ReaderT RPCServerContext IO a

route :: RPCRequest -> RPCServer (Maybe ByteString)
route (RPCIndexCommand ci mtime)             = sendIndex_ $ indexRequestForCommand ci mtime
route (RPCIndexFile sf mtime)                = sendIndex_ $ indexRequestForUpdate sf mtime
route (RPCGetCommandInfo sf)                 = sendQuery $ DBGetCommandInfo sf
route (RPCGetSimilarCommandInfo sf)          = sendQuery $ DBGetSimilarCommandInfo sf
route (RPCGetDefinition sl)                  = sendQuery $ DBGetDefinition sl
route (RPCGetCallers sl)                     = sendQuery $ DBGetCallers sl
route (RPCGetCallees sl)                     = sendQuery $ DBGetCallees sl
route (RPCGetBases sl)                       = sendQuery $ DBGetBases sl
route (RPCGetOverrides sl)                   = sendQuery $ DBGetOverrides sl
route (RPCGetMembers sl)                     = sendQuery $ DBGetMembers sl
route (RPCGetRefs sl)                        = sendQuery $ DBGetRefs sl
route (RPCGetReferenced sl)                  = sendQuery $ DBGetReferenced sl
route (RPCGetDeclReferenced sl)              = sendQuery $ DBGetDeclReferenced sl
route (RPCGetHierarchy sl)                   = sendQuery $ DBGetHierarchy sl
route (RPCGetInclusions sf)                  = sendQuery $ DBGetInclusions sf
route (RPCGetIncluders sf)                   = sendQuery $ DBGetIncluders sf
route (RPCGetInclusionHierarchy sf)          = sendQuery $ DBGetInclusionHierarchy sf
route (RPCUpdateAndFindDirtyInclusions s is) = doUpdateInclusions s is
route (RPCWait)                              = sendWait_
route (RPCLog s)                             = logInfo s >> return Nothing
route (RPCPing)                              = return . Just $! encode (RPCOK ())
route (RPCFoundUpdates _)                    = error "Should not route RPCFoundUpdates"
route (RPCDone)                              = error "Should not route RPCDone"
route (RPCStop)                              = error "Should not route RPCStop"

sendIndex_ :: IndexRequest -> RPCServer (Maybe ByteString)
sendIndex_ !req = do
  ctx <- ask
  lift $ atomically $ addPendingIndex (rsIndexStream ctx) req
  return Nothing

sendQuery :: Serialize a => (Response a -> DBRequest) -> RPCServer (Maybe ByteString)
sendQuery !req = do
  ctx <- ask
  result <- callLenChan (rsDBQueryChan ctx) req
  return . Just $! encode (RPCOK result)

doUpdateInclusions :: SourceFileHash -> [Inclusion] -> RPCServer (Maybe ByteString)
doUpdateInclusions !s !is = do
  ctx <- ask
  result <- lift $ updateInclusions (rsIndexStream ctx) (rsDBUpdateChan ctx) s is
  return . Just $! encode (RPCOK result)

sendWait_ :: RPCServer (Maybe ByteString)
sendWait_ = do
  logInfo "Sending barrier request"
  ctx <- ask
  result <- callLenChan (rsIdleChan ctx) IdleBarrier
  return . Just $! encode (RPCOK result)

data RPCServerExit = RPCServerExit deriving (Show, Typeable)
instance Exception RPCServerExit
