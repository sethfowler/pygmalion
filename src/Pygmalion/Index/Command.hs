{-# LANGUAGE ViewPatterns #-}

module Pygmalion.Index.Command
( getCommandInfo
) where

import Control.Applicative
import qualified Data.ByteString.UTF8 as B
import Data.List
import Data.Maybe
import Data.Traversable
import System.Path

import Pygmalion.Core
import Pygmalion.File
import Pygmalion.Index.Extension

getCommandInfo :: String -> [String] -> String -> IO (Maybe CommandInfo)
getCommandInfo cmd args wd = do
    let sourceFile = findSourceFile args >>= absNormPath wd
    realSF <- traverse (canonicalPath . B.fromString) sourceFile
    let finalArgs = map B.fromString . addStdlibArg . absArgs wd . filterArgs $ args
    return $ flip fmap realSF $ \sf ->
      CommandInfo sf
                  (B.fromString wd)
                  (B.fromString cmd)
                  finalArgs
                  (inferLang args $ B.toString sf)

inferLang :: [String] -> String -> Language
inferLang as f = fromMaybe UnknownLanguage $ inferLangFromArgs as <|>
                                             (Just . extensionLanguage) f

inferLangFromArgs :: [String] -> Maybe Language
inferLangFromArgs ("-x" : "c" : _)              = Just CLanguage
inferLangFromArgs ("-x" : "c-header" : _)       = Just CLanguage
inferLangFromArgs ("-x" : "cpp-output" : _)     = Just CLanguage
inferLangFromArgs ("-x" : "c++" : _)            = Just CPPLanguage
inferLangFromArgs ("-x" : "c++-header" : _)     = Just CPPLanguage
inferLangFromArgs ("-x" : "c++-cpp-output" : _) = Just CPPLanguage
inferLangFromArgs (_ : as)                      = inferLangFromArgs as
inferLangFromArgs []                            = Nothing

findSourceFile :: [String] -> Maybe String
findSourceFile [] = Nothing
findSourceFile ("-main-file-name" : _ : as) = findSourceFile as
findSourceFile (a : as) | hasSourceExtension a = Just a
                        | otherwise = findSourceFile as

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

-- We need to convert relative paths to absolute paths, because we may use these
-- arguments in analyses that aren't running from the same working directory.
-- FIXME: Terrible code.
absArgs :: FilePath -> [String] -> [String]
absArgs wd ("-I" : p : as) = "-I" : cleanPath wd p : absArgs wd as
absArgs wd ((stripPrefix "-I" -> Just p) : as) = ("-I" ++ (cleanPath wd p)) : absArgs wd as
absArgs wd ("-include" : p : as) = "-include" : cleanPath wd p : absArgs wd as
absArgs wd ((stripPrefix "-include" -> Just p) : as) = ("-include" ++ (cleanPath wd p)) : absArgs wd as
absArgs wd ("-imacros" : p : as) = "-imacros" : cleanPath wd p : absArgs wd as
absArgs wd ((stripPrefix "-imacros" -> Just p) : as) = ("-imacros" ++ (cleanPath wd p)) : absArgs wd as
absArgs wd ("-isystem" : p : as) = "-isystem" : cleanPath wd p : absArgs wd as
absArgs wd ((stripPrefix "-isystem" -> Just p) : as) = ("-isystem" ++ (cleanPath wd p)) : absArgs wd as
absArgs wd ("-iquote" : p : as) = "-iquote" : cleanPath wd p : absArgs wd as
absArgs wd ((stripPrefix "-iquote" -> Just p) : as) = ("-iquote" ++ (cleanPath wd p)) : absArgs wd as
absArgs wd ("-isysroot" : p : as) = "-isysroot" : cleanPath wd p : absArgs wd as
absArgs wd ((stripPrefix "-isysroot" -> Just p) : as) = ("-isysroot" ++ (cleanPath wd p)) : absArgs wd as
absArgs wd ((stripPrefix "--sysroot=" -> Just p) : as) = ("--sysroot=" ++ (cleanPath wd p)) : absArgs wd as
absArgs wd (a : as) = a : absArgs wd as
absArgs _ [] = []

-- On OS X 10.9, Apple decided to put libstdc++'s headers in
-- /usr/include/c++/4.2.1, but libc++'s headers only in
-- /Library/Developer/CommandLineTools/usr/lib/c++/v1. Since the
-- latter is not a standard location, attempts to use libc++ will fail
-- with missing header errors unless we manually add the appropriate
-- directory to the include search path. Rather than support that,
-- though, at the moment I fall back to forcing libstdc++, because
-- unfortunately given the choice libclang will default to libc++.
-- TODO: Make this more robust.
addStdlibArg :: [String] -> [String]
addStdlibArg as = "-stdlib=libstdc++" : as

cleanPath :: FilePath -> FilePath -> FilePath
cleanPath _ p@('=' : _) = p  -- Ignore bizarre GCC sysroot syntax.
cleanPath wd p = fromMaybe p $ absNormPath wd p
