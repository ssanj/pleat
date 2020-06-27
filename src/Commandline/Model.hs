{-# LANGUAGE DerivingStrategies #-}

module Commandline.Model
       (
          -- Data types
          OptionStatus(..)
       ,  PleatCommand(..)
       ,  PleatVersion(..)
       ,  PleatGitHash(..)
       ,  VersionInfo(..)
       ) where

import Config (Config)

data OptionStatus = Enabled | Disabled deriving stock (Eq, Show)

data PleatCommand = PleatConfigCommand Config | PleatVersionCommand deriving stock (Eq, Show)

newtype PleatVersion = PleatVersion String deriving stock (Eq, Show)

newtype PleatGitHash = PleatGitHash String deriving stock (Eq, Show)

data VersionInfo = VersionInfo { pleatVersion :: PleatVersion, _pleatGitHash :: PleatGitHash } deriving stock (Eq, Show)