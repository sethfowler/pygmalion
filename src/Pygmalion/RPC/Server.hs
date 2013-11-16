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

runRPCServer :: Config -> IndexStream -> DBUpdateChan -> DBQueryChan -> IdleChan -> IO ()
runRPCServer cf iStream dbUpdateChan dbQueryChan idleChan = do
    conns <- newMVar 0
    threadId <- myThreadId
    runTCPServer settings (serverApp $ RPCServerContext threadId
                                                        iStream
                                                        dbUpdateChan
                                                        dbQueryChan
                                                        idleChan
                                                        conns)
  where
    settings = serverSettings port addr :: ServerSettings IO
    port = ifPort cf
    addr = fromString $ ifAddr cf

serverApp :: RPCServerContext -> Application IO
serverApp ctx ad = do
    open
    conduit `onException` closeIO
  where
    getCI = {-# SCC "serverget" #-} get :: Get RPCRequest

    conduit = appSource ad $= conduitGet getCI =$= process [] $$ appSink ad

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
      when (newConns < 0) $
        throwTo (rsThread ctx) RPCServerExit -- Terminate the server.

    process ups = do
      result <- await
      case result of
        Just RPCDone                 -> liftIO (sendUpdates ctx ups) >> close
        Just RPCStop                 -> liftIO (sendUpdates ctx ups) >> terminate
        Just (RPCFoundDef !df)       -> process (DBUpdateDef df : ups)
        Just (RPCFoundRef !ru)       -> process (DBUpdateRef ru : ups)
        Just (RPCFoundOverride !ov)  -> process (DBUpdateOverride ov : ups)
        Just (RPCFoundInclusion !ic) -> process (DBUpdateInclusion ic : ups)
        Just req                     -> do liftIO (sendUpdates ctx ups)
                                           res <- liftIO $ runReaderT (route req) ctx
                                           case res of
                                              Just res' -> yield res' >> process ups
                                              Nothing   -> process ups
        _                            -> do logError "RPC server got bad request"
                                           yield . encode $ (RPCError :: RPCResponse ())
                                           liftIO (sendUpdates ctx ups)
                                           close

sendUpdates :: RPCServerContext -> [DBUpdate] -> IO ()
sendUpdates ctx ups = writeLenChan (rsDBUpdateChan ctx) ups

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
route (RPCIndexCommand ci)          = sendIndex_ $ FromBuild ci False
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
route (RPCWait)                     = sendWait_
route (RPCLog s)                    = logInfo s >> return Nothing
route (RPCPing)                     = return . Just $! encode (RPCOK ())
route (RPCFoundDef _)               = error "Should not route RPCFoundDef"
route (RPCFoundRef _)               = error "Should not route RPCFoundRef"
route (RPCFoundOverride _)          = error "Should not route RPCFoundOverride"
route (RPCFoundInclusion _)         = error "Should not route RPCFoundInclusion"
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

sendWait_ :: RPCServer (Maybe ByteString)
sendWait_ = do
  logInfo "Sending barrier request"
  ctx <- ask
  result <- callLenChan (rsIdleChan ctx) IdleBarrier
  return . Just $! encode (RPCOK result)

data RPCServerExit = RPCServerExit deriving (Show, Typeable)
instance Exception RPCServerExit
