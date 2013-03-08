module Pygmalion.Analyze
( scanCommandAndUpdateDB
, runAnalysisThread
) where

import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Maybe
import System.FilePath.Posix

import Pygmalion.Analyze.Command
import Pygmalion.Analyze.Source
import Pygmalion.Core
import Pygmalion.Database

runAnalysisThread :: Chan (Maybe CommandInfo) -> IO ()
runAnalysisThread chan = do
    commandStream <- getChanContents chan
    mapM_ analyze $ takeWhile isJust commandStream
  where analyze cmd = scanCommandAndUpdateDB (fromJust cmd)

scanCommandAndUpdateDB :: CommandInfo -> IO (Maybe ())
scanCommandAndUpdateDB cmdInfo = runScanner (    analyzeCode cmdInfo
                                             >>= updateDB)

type Scanner a = MaybeT IO a

runScanner :: Scanner a -> IO (Maybe a)
runScanner a = runMaybeT a

analyzeCode :: CommandInfo -> Scanner (CommandInfo, [FilePath])
analyzeCode ci = do
  -- liftIO $ putStrLn $ "Analyzing " ++ (show ci)
  includedFiles <- liftIO $ clangGetIncludes ci
  let localHeaders = filter isLocalHeader includedFiles
  return (ci, map normalise localHeaders)

updateDB :: (CommandInfo, [FilePath]) -> Scanner ()
updateDB (ci, headers) = liftIO $ withDB dbFile $ \handle -> do
    updateRecord handle ci
    -- Add entries for all included non-system headers, using the same metadata.
    forM_ headers $ \header -> do
      updateRecord handle $ updateSourceFile ci header
