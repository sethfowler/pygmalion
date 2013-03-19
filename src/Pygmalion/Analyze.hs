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
runAnalysisThread chan = withDB dbFile $ \h -> do
    commandStream <- getChanContents chan
    mapM_ (scanCommandAndUpdateDB h . fromJust) $ takeWhile isJust commandStream

scanCommandAndUpdateDB :: DBHandle -> CommandInfo -> IO (Maybe ())
scanCommandAndUpdateDB h cmdInfo = runScanner $    analyzeCode cmdInfo
                                               >>= updateDB h

type Scanner a = MaybeT IO a

runScanner :: Scanner a -> IO (Maybe a)
runScanner a = runMaybeT a

analyzeCode :: CommandInfo -> Scanner (CommandInfo, [FilePath])
analyzeCode ci = do
  -- liftIO $ putStrLn $ "Analyzing " ++ (show ci)
  includedFiles <- liftIO $ clangGetIncludes ci
  case includedFiles of
    Just fs -> return (ci, (map normalise) . (filter isLocalHeader) $ fs)
    Nothing -> MaybeT $ return Nothing

updateDB :: DBHandle -> (CommandInfo, [FilePath]) -> Scanner ()
updateDB h (ci, headers) = liftIO $ do
    updateRecord h ci
    -- Add entries for all included non-system headers, using the same metadata.
    forM_ headers $ \header -> do
      updateRecord h $ updateSourceFile ci header
