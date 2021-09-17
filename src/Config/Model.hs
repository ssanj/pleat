{-# LANGUAGE DerivingStrategies #-}

module Config.Model
       (
           -- Data Types
           Config(..)
        ,  MaxPathLength(..)
        ,  PleatOption(..)
        ,  HostnameOption(..)
        ,  PathOption(..)
        ,  Hostname(..)
        ,  GitOption(..)
        ,  TimestampOption(..)
        ,  Prompt(..)
        ,  PromptSeparator(..)
        ) where

import qualified Data.Text as T

newtype HostnameOption  = HostnameOption { _overrideHostname ::  Maybe Hostname } deriving stock (Eq, Show)

newtype Hostname        = Hostname { _hostname :: T.Text } deriving stock (Eq, Show)

newtype Prompt          = Prompt { _prompt :: T.Text } deriving stock (Eq, Show)

newtype PromptSeparator = PromptSeparator { _promptSeparator :: T.Text } deriving stock (Eq, Show)

newtype MaxPathLength   = MaxPathLength { _pathLength :: Int } deriving stock (Eq, Show)

newtype PathOption      = PathOption { _maxPathLength :: MaxPathLength } deriving stock (Eq, Show)

data PleatOption a   = OptionOff | OptionOn a deriving stock (Eq, Show)

data GitOption       = GitOption deriving stock (Eq, Show)

data TimestampOption = TimestampOption deriving stock (Eq, Show)

data Config = Config {
   _pleatHostnameOverrideOption :: PleatOption HostnameOption
,  _pleatPathOption             :: PleatOption PathOption
,  _pleatGitOption              :: PleatOption GitOption
,  _pleatTimestampOption        :: PleatOption TimestampOption
,  _pleatPrompt                 :: Prompt
,  _pleatPromptSeparator        :: PromptSeparator
} deriving stock (Eq, Show)
