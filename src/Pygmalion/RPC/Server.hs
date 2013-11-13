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
import Data.Conduit.Network
import Data.Serialize
import Data.String
import Data.Typeable

import Control.Concurrent.MVar
import Control.Concurrent.Chan.Len
import Pygmalion.Config
import Pygmalion.Database.Request
import Pygmalion.Idle
import Pygmalion.Index.Request
import Pygmalion.Index.Stream
import Pygmalion.Log
import Pygmalion.RPC.Request

runRPCServer :: Config -> IndexStream -> DBChan -> DBChan -> IdleChan -> IO ()
runRPCServer cf iStream dbChan dbQueryChan idleChan = do
    conns <- newMVar 0
    threadId <- myThreadId
    runTCPServer settings (serverApp $ RPCServerContext threadId
                                                        iStream
                                                        dbChan
                                                        dbQueryChan
                                                        idleChan
                                                        conns)
  where
    settings = (serverSettings port addr) :: ServerSettings IO
    port = ifPort cf
    addr = fromString $ ifAddr cf

serverApp :: RPCServerContext -> Application IO
serverApp ctx ad = do
    open
    conduit `onException` closeIO
  where
    getCI = {-# SCC "serverget" #-} get :: Get RPCRequest

    conduit = (appSource ad) $= conduitGet getCI =$= process $$ (appSink ad)

    open = liftIO $ modifyMVar_ (rsConnections ctx) (\c -> return (c + 1))
    close = liftIO $ terminate' 1
    closeIO = terminate' 1
    terminate = liftIO $ terminate' 2

    terminate' v = do
      -- If v == 1, this balances the increment to rsConnections that
      -- happened when we opened this connection. If v > 1, this
      -- ensures that rsConnections will eventually drop below 0,
      -- terminating the server.
      conns <- takeMVar (rsConnections ctx)
      let newConns = conns - v
      putMVar (rsConnections ctx) newConns
      when (newConns < 0) $ do
        throwTo (rsThread ctx) RPCServerExit -- Terminate the server.

    process = do
      result <- await
      case result of
        Just RPCDone -> close
        Just RPCStop -> terminate
        Just req     -> do res <- liftIO $ runReaderT (route req) ctx
                           case res of
                              Just res' -> yield res' >> process
                              Nothing   -> process
        _            -> do logError "RPC server got bad request"
                           yield . encode $ (RPCError :: RPCResponse ())
                           close

data RPCServerContext = RPCServerContext
  { rsThread      :: !ThreadId
  , rsIndexStream :: !IndexStream
  , rsDBChan      :: !DBChan
  , rsDBQueryChan :: !DBChan
  , rsIdleChan    :: !IdleChan
  , rsConnections :: !(MVar Int)
  }
type RPCServer a = ReaderT RPCServerContext IO a

route :: RPCRequest -> RPCServer (Maybe ByteString)
route (RPCIndexCommand ci)          = sendIndex_ $ FromBuild ci
route (RPCIndexFile ci)             = sendIndex_ $ FromNotify ci
route (RPCGetCommandInfo sf)        = sendQuery $ DBGetCommandInfo sf
route (RPCGetSimilarCommandInfo sf) = sendQuery $ DBGetSimilarCommandInfo sf
route (RPCGetDefinition sl)         = sendQuery $ DBGetDefinition sl
route (RPCGetCallers sl)            = sendQuery $ DBGetCallers sl
route (RPCGetCallees sl)            = sendQuery $ DBGetCallees sl
route (RPCGetBases sl)              = sendQuery $ DBGetBases sl
route (RPCGetOverrides sl)          = sendQuery $ DBGetOverrides sl
route (RPCGetMembers sl)            = sendQuery $ DBGetMembers sl
route (RPCGetRefs sl)               = sendQuery $ DBGetRefs sl
route (RPCGetReferenced sl)         = sendQuery $ DBGetReferenced sl
route (RPCGetDeclReferenced sl)     = sendQuery $ DBGetDeclReferenced sl
route (RPCGetHierarchy sl)          = sendQuery $ DBGetHierarchy sl
route (RPCGetInclusions sf)         = sendQuery $ DBGetInclusions sf
route (RPCGetIncluders sf)          = sendQuery $ DBGetIncluders sf
route (RPCGetInclusionHierarchy sf) = sendQuery $ DBGetInclusionHierarchy sf
route (RPCFoundDef df)              = sendUpdate_ $ DBUpdateDef df
route (RPCFoundOverride ov)         = sendUpdate_ $ DBUpdateOverride ov
route (RPCFoundRef ru)              = sendUpdate_ $ DBUpdateRef ru
route (RPCFoundInclusion ic)        = sendUpdate_ $ DBUpdateInclusion ic
route (RPCWait)                     = sendWait_
route (RPCLog s)                    = logInfo s >> return Nothing
route (RPCPing)                     = return . Just $! encode (RPCOK ())
route (RPCDone)                     = error "Should not route RPCDone"
route (RPCStop)                     = error "Should not route RPCStop"

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

sendUpdate_ :: DBRequest -> RPCServer (Maybe ByteString)
sendUpdate_ !req = do
  ctx <- ask
  writeLenChan (rsDBChan ctx) req
  return Nothing

sendWait_ :: RPCServer (Maybe ByteString)
sendWait_ = do
  logInfo "Sending barrier request"
  ctx <- ask
  result <- callLenChan (rsIdleChan ctx) IdleBarrier
  return . Just $! encode (RPCOK result)

data RPCServerExit = RPCServerExit deriving (Show, Typeable)
instance Exception RPCServerExit
