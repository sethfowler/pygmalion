{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.List
import Data.Time.Clock
import qualified Filesystem.Path.CurrentOS as FP
import System.Directory
import System.FSNotify

import Pygmalion.Analyze.Extension

main :: IO ()
main = withManager watch

watch :: WatchManager -> IO ()
watch m = do
  curDir <- getCurrentDirectory
  putStrLn $ "Started watching " ++ (show curDir) ++ "."
  watchDir m (FP.decodeString curDir) (const True) handleEvent
  paths <- getDirectoryContents curDir
  forM_ (filter isValid paths) $ \p -> do
    putStrLn $ "Started watching " ++ (show p) ++ "."
    watchTree m (FP.decodeString p) (const True) handleEvent
  _ <- getLine
  putStrLn "Stopped watching."

handleEvent :: Event -> IO ()
handleEvent (Added f t)    | isInteresting (FP.encodeString f) = handleInteresting f t
handleEvent (Modified f t) | isInteresting (FP.encodeString f) = handleInteresting f t
handleEvent (Removed f t)  | isInteresting (FP.encodeString f) = handleInteresting f t
handleEvent _ = return ()

handleInteresting :: FP.FilePath -> UTCTime -> IO ()
handleInteresting f t = putStrLn $ (show f) ++ " was touched at " ++ (show t)

isValid :: FilePath -> Bool
isValid = (not . isPrefixOf ".")

isInteresting :: FilePath -> Bool
isInteresting f = hasSourceExtension f || hasHeaderExtension f
