module Pygmalion.File
( getMTime
) where

import Control.Exception
import Data.Time.Clock.POSIX
import System.Directory

import Pygmalion.Core
import Pygmalion.Log

getMTime :: SourceFile -> IO (Maybe Time)
getMTime sf = do
  result <- try $ getModificationTime (unSourceFile sf)
  case result of
    Right clockTime -> return . Just . floor . utcTimeToPOSIXSeconds $ clockTime
    Left e          -> do logInfo $ "Couldn't read mtime for file "
                                 ++ unSourceFile sf ++ ": "
                                 ++ show (e :: IOException)
                          return Nothing  -- Most likely the file has been deleted.
