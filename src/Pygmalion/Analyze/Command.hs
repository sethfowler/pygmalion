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

import Data.Bool.Predicate
import Pygmalion.Core

sourceExtensions, headerExtensions :: [String]
sourceExtensions = [".c", ".cc", ".cpp", ".C"]
headerExtensions = [".h", ".hh", ".hpp", ".H"]

hasSourceExtension, hasHeaderExtension, isLocalHeader :: String -> Bool
hasSourceExtension a = any (`isSuffixOf` a) sourceExtensions
hasHeaderExtension a = any (`isSuffixOf` a) headerExtensions
isLocalHeader = isRelative .&&. isValid .&&. hasHeaderExtension

getCommandInfo :: Command -> IO (Maybe CommandInfo)
getCommandInfo (Command c as) = do
    wd <- getCurrentDirectory
    time <- getPOSIXTime
    case sourceFile of
      Just sf -> return . Just $ CommandInfo sf wd (Command c fas) (floor time)
      _       -> return Nothing
  where sourceFile = find hasSourceExtension as
        fas        = filterArgs as

-- We need to filter arguments that cause dependency files to be generated,
-- as they'll gum up the works when we use libclang to analyze files later.
-- We also need to remove the source file itself as libclang doesn't like it,
-- and any '-c' and '-o' flags.
filterArgs :: [String] -> [String]
filterArgs ("-c" : as) = filterArgs as
filterArgs ("-o" : _ : as) = filterArgs as
filterArgs ("-M" : as) = filterArgs as
filterArgs ("-MM" : as) = filterArgs as
filterArgs ("-MG" : as) = filterArgs as
filterArgs ("-MP" : as) = filterArgs as
filterArgs ("-fpch-deps" : as) = filterArgs as
filterArgs ("-MT" : _ : as) = filterArgs as
filterArgs ("-MQ" : _ : as) = filterArgs as
filterArgs ("-MD" : as) = filterArgs as
filterArgs ("-MMD" : as) = filterArgs as
filterArgs ("-MF" : _ : as) = filterArgs as
filterArgs (a : as) | "-W" `isPrefixOf` a && "-MD" `isInfixOf` a = filterArgs as
filterArgs (a : as) | hasSourceExtension a = filterArgs as
filterArgs (a : as) = a : filterArgs as
filterArgs [] = []
