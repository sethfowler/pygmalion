{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pygmalion.Config
( Config(..)
, getConfiguration
) where

import Control.Applicative
import Control.Monad
import Data.Yaml
import qualified Data.ByteString as B

import Pygmalion.Core
import Pygmalion.Log

-- The make command is a shell command specified as a format string
-- with the following substitutions:
-- $(idx)      - The indexer.
-- $(idx-args) - Arguments for the indexer.
-- $(cc)       - The C compiler.
-- $(cc-args)  - Arguments for the C compiler.
-- $(cpp)      - The C++ compiler.
-- $(cpp-args) - Arguments for the C++ compiler.
-- $(mk-args)  - Commandline arguments for pygmake.
--
-- A default format string for GNU make is below.
-- For CMake, this would work:
-- cmake $(mk-args)
--   -DCMAKE_C_COMPILER="$(idx)"
--   -DCMAKE_C_FLAGS="$(idx-args) $(cc) $(cc-args)"
--   -DCMAKE_CXX_COMPILER="$(idx)"
--   -DCMAKE_CXX_FLAGS="$(idx-args) $(cxx) $(cxx-args)"

data Config = Config
  { ifAddr    :: String   -- Address of interface to bind to.
  , ifPort    :: Port     -- Port to bind to.
  , makeCmd   :: String   -- Format string for the make command. See above.
  , makeCDB   :: Bool     -- If true, pygmake generates a CDB automatically.
  , makeTAGS  :: Bool     -- If true, pygmake generates a TAGS file automatically.
  , cc        :: String   -- C compiler executable to use.
  , ccArgs    :: [String] -- Extra C compiler args, if any.
  , cpp       :: String   -- C++ compiler executable to use.
  , cppArgs   :: [String] -- Extra C++ compiler args, if any.
  , logLevel  :: Priority -- The level of logging to enable.
  } deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config
  { ifAddr    = "127.0.0.1"
  , ifPort    = 7999
  , makeCmd   = "make CC=\"$(idx) $(idx-args) $(cc) $(cc-args)\" " ++
                "CXX=\"$(idx) $(idx-args) $(cpp) $(cpp-args)\" $(mk-args)"
  , makeCDB   = False
  , makeTAGS  = False
  , cc        = "clang"
  , ccArgs    = []
  , cpp       = "clang++"
  , cppArgs   = []
  , logLevel  = INFO
  }

instance FromJSON Priority where
  parseJSON (String s)
    | s == "debug"     = return DEBUG
    | s == "info"      = return INFO
    | s == "notice"    = return NOTICE
    | s == "warning"   = return WARNING
    | s == "error"     = return ERROR
    | s == "critical"  = return CRITICAL
    | s == "alert"     = return ALERT
    | s == "emergency" = return EMERGENCY
  parseJSON _ = mzero

instance FromJSON Config where
  parseJSON (Object o) =
    Config <$> o .:? "address"              .!= (ifAddr defaultConfig)
           <*> o .:? "port"                 .!= (ifPort defaultConfig)
           <*> o .:? "make-command"         .!= (makeCmd defaultConfig)
           <*> o .:? "compilation-database" .!= (makeCDB defaultConfig)
           <*> o .:? "tags"                 .!= (makeTAGS defaultConfig)
           <*> o .:? "cc"                   .!= (cc defaultConfig)
           <*> o .:? "cc-args"              .!= (ccArgs defaultConfig)
           <*> o .:? "cpp"                  .!= (cpp defaultConfig)
           <*> o .:? "cpp-args"             .!= (cppArgs defaultConfig)
           <*> o .:? "log-level"            .!= (logLevel defaultConfig)
  parseJSON _ = mzero

readConfigFile :: IO B.ByteString
readConfigFile = B.readFile configFile

reportError :: String -> IO a
reportError err = error $ "Couldn't parse configuration file: " ++ err

checkConfig :: Config -> IO Config
checkConfig conf@Config { ifPort = port }
  | port == 0 = reportError "Port 0 is invalid"
  | otherwise = return conf

getConfiguration :: IO Config
getConfiguration = do
  result <- (decodeEither' <$> readConfigFile)
  either (reportError . show) checkConfig result
