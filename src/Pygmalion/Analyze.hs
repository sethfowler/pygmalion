module Pygmalion.Analyze
( scanCommandAndUpdateDB
, runAnalysisThread
) where

import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Maybe
import qualified Data.Text as T

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

analyzeCode :: CommandInfo -> Scanner (CommandInfo, [FilePath], [DefInfo])
analyzeCode ci = do
  --liftIO $ putStrLn $ "Analyzing " ++ (show ci)
  result <- liftIO $ runSourceAnalyses ci
  case result of
    Just (is, ds) -> return (ci, is, ds)
    Nothing -> MaybeT $ return Nothing

updateDB :: DBHandle -> (CommandInfo, [FilePath], [DefInfo]) -> Scanner ()
updateDB h (ci, includes, defs) = liftIO $ do
    updateSourceFile h ci
    -- Update entries for all non-system includes, using the same metadata.
    forM_ includes $ \i -> do
      updateSourceFile h $ withSourceFile ci (T.pack i)
    -- Update entries for all definitions.
    forM_ defs $ \d -> do
      updateDef h d
