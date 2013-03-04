module Pygmalion.RPC.Server
( runServer
) where

import Prelude hiding (catch)
import Control.Concurrent
import Control.Exception
import Control.Monad.Trans
import Network.MessagePackRpc.Server

--import Pygmalion.Analyze
--import Pygmalion.Core

pretendToScan :: (FilePath, [String]) -> Method ()
pretendToScan (f, c) = liftIO $ putStrLn $ "Got db path [" ++ f ++
                                           "] and command [" ++ (show c) ++ "]"

runServer :: (a -> IO b) -> a -> IO b
runServer f v = do
  thread <- forkIO serverMain
  ret <- (f v) `catch` (\e ->
                 error $ "runServer: IO action threw" ++ (show (e :: SomeException)))
  killThread thread
  return ret

serverMain :: IO ()
serverMain = do
  serve 8081
    [
      ("scan", toMethod pretendToScan)
    ]
