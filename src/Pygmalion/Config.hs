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
    makeArgs :: [String], -- Extra make args, if any.
    makeCDB :: Bool,      -- If true, pygmake generates a CDB automatically.
    cc :: String,         -- C compiler executable to use.
    ccArgs :: [String],   -- Extra C compiler args, if any.
    cpp :: String,        -- C++ compiler executable to use.
    cppArgs :: [String]   -- Extra C++ compiler args, if any.
  } deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config
  {
    ifAddr   = "127.0.0.1",
    ifPort   = 0, -- Meaning to use an arbitrary port.
    make     = "make",
    makeArgs = [],
    makeCDB  = False,
    cc       = "clang",
    ccArgs   = [],
    cpp      = "clang++",
    cppArgs  = []
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
    cfMakeCDB   <- confValue conf "make.compilation-database" makeCDB
    cfCC        <- confValue conf "c.compiler" cc
    cfCCArgs    <- confValue conf "c.args" ccArgs
    cfCPP       <- confValue conf "cpp.compiler" cpp
    cfCPPArgs   <- confValue conf "cpp.args" cppArgs

    return $ Config cfInterface cfPort
                    cfMake cfMakeArgs cfMakeCDB
                    cfCC cfCCArgs
                    cfCPP cfCPPArgs
  where
    confValue conf name f = lookupDefault (f defaultConfig) conf name
