{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pygmalion.Config
( Config(..)
, getConfiguration
) where

import Data.Configurator
import qualified Data.Configurator.Types as CT
import Data.Text

import Pygmalion.Core

data Config = Config
  {
    ifAddr :: String,     -- Address of interface to bind to.
    ifPort :: Port,       -- Port to bind to.
    make :: String,       -- Make executable to use.
    makeArgs :: [String], -- Extra make args, if any. (TODO)
    cc :: String,         -- C compiler executable to use.
    ccArgs :: [String],   -- Extra C compiler args, if any. (TOD)
    cpp :: String,        -- C++ compiler executable to use.
    cppArgs :: [String],  -- Extra C++ compiler args, if any. (TODO)
    relativeCDB :: Bool   -- Use relative paths in compilation database? (TODO)
  } deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config
  {
    ifAddr = "127.0.0.1",
    ifPort = 0, -- Meaning to use an arbitrary port.
    make = "make",
    makeArgs = [],
    cc = "clang",
    ccArgs = [],
    cpp = "clang++",
    cppArgs = [],
    relativeCDB = False
  }

instance CT.Configured [String] where
  convert (CT.List l) = Just $ convert' l
    where convert' (CT.String s : vs) = unpack s : convert' vs
          convert' (v : vs) = show v : convert' vs
          convert' [] = []
  convert _ = Nothing

getConfiguration :: IO Config
getConfiguration = do
    conf <- load [Optional $ "$(HOME)/" ++ configFile, Optional configFile]

    cfInterface <- confValue conf "network.interface" ifAddr
    cfPort      <- confValue conf "network.port" ifPort
    cfMake      <- confValue conf "make.command" make
    cfMakeArgs  <- confValue conf "make.args" makeArgs
    cfCC        <- confValue conf "c.compiler" cc
    cfCCArgs    <- confValue conf "c.args" ccArgs
    cfCPP       <- confValue conf "cpp.compiler" cpp
    cfCPPArgs   <- confValue conf "cpp.args" cppArgs
    cfRelCDB    <- confValue conf "compilation-database.relative-paths" relativeCDB

    return $ Config cfInterface cfPort
                    cfMake cfMakeArgs
                    cfCC cfCCArgs
                    cfCPP cfCPPArgs
                    cfRelCDB
  where
    confValue conf name f = lookupDefault (f defaultConfig) conf name
