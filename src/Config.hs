{-# LANGUAGE DerivingStrategies #-}

module Config 
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
           -- Functions
        ,  defaultMaxPathLength
        ,  defaultPrompt
        ) where

-- import qualified Parser.GitParser as GP

newtype HostnameOption = HostnameOption { _overrideHostname ::  Maybe Hostname } deriving stock (Eq, Show)
newtype Hostname       = Hostname { _hostname :: String } deriving stock (Eq, Show)
newtype Prompt         = Prompt { _prompt :: String } deriving stock (Eq, Show)
newtype MaxPathLength  = MaxPathLength { _pathLength :: Int } deriving stock (Eq, Show)
newtype PathOption  = PathOption { _maxPathLength :: MaxPathLength } deriving stock (Eq, Show)

data PleatOption a   = OptionOff | OptionOn a deriving stock (Eq, Show)
data GitOption       = GitOption deriving stock (Eq, Show)
data TimestampOption = TimestampOption deriving stock (Eq, Show)


data Config = Config { 
   _pleatHostnameOption  :: PleatOption HostnameOption
,  _pleatPathOption      :: PleatOption PathOption 
,  _pleatGitOption       :: PleatOption GitOption
,  _pleatTimestampOption :: PleatOption TimestampOption
,  _pleatPrompt          :: Prompt
} deriving stock (Eq, Show)


defaultMaxPathLength :: MaxPathLength
defaultMaxPathLength = MaxPathLength 50

defaultPrompt :: Prompt
defaultPrompt = Prompt "> "