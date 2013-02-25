module Pygmalion.Analyze.Command
( hasSourceExtension
, hasHeaderExtension
, isLocalHeader
, getCommandInfo
) where

import Data.List
import Data.Time.Clock.POSIX
import System.Directory
import System.FilePath.Posix

import Pygmalion.Core

sourceExtensions = [".c", ".cc", ".cpp", ".C"]
headerExtensions = [".h", ".hh", ".hpp", ".H"]

hasSourceExtension a = any (`isSuffixOf` a) sourceExtensions
hasHeaderExtension a = any (`isSuffixOf` a) headerExtensions
isLocalHeader = isRelative .&&. isValid .&&. hasHeaderExtension

getCommandInfo :: Command -> IO (Maybe CommandInfo)
getCommandInfo cmd@(c : as) = do
    wd <- getCurrentDirectory
    time <- getPOSIXTime
    case sourceFile of
      Just sf -> return . Just $ CommandInfo sf wd (c : filteredArgs) (floor time)
      _       -> return Nothing
  where sourceFile   = find hasSourceExtension as
        filteredArgs = filterArgs as

-- We need to filter arguments that cause dependency files to be generated,
-- as they'll gum up the works when we use libclang to analyze files later.
-- We also need to remove the source file itself as libclang doesn't like it. 
filterArgs :: [String] -> [String]
filterArgs ("-MD" : as) = filterArgs as
filterArgs ("-MF" : a : as) = filterArgs as
filterArgs (a : as) | "-W" `isPrefixOf` a && "-MD" `isInfixOf` a = filterArgs as
filterArgs (a : as) | hasSourceExtension a = filterArgs as
filterArgs (a : as) = a : filterArgs as
filterArgs [] = []

(.&&.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(.&&.) f g v = f v && g v
