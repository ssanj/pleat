{-# LANGUAGE DerivingStrategies #-}

module Config 
       (
           -- Data Types
           Config(..)
        ,  MaxPathLength(..)
        ,  PleatOption(..)
        ,  HostnameOption(..)
        ,  Hostname(..)        
        ,  GitOption(..)        
           -- Functions
        ,  defaultMaxPathLength
        ) where

data PleatOption a = OptionOff | OptionOn a deriving stock (Eq, Show)

data HostnameOption = HostnameOption { _overrideHostname ::  Maybe Hostname } deriving stock (Eq, Show)
newtype Hostname = Hostname { _hostname :: String } deriving stock (Eq, Show)

data GitOption = GitOption deriving stock (Eq, Show)

data Config = Config { 
   _pleatHostnameOption :: PleatOption HostnameOption
,  _maxPathLength :: MaxPathLength 
,  _pleatGitOption :: PleatOption GitOption
} deriving stock (Eq, Show)

newtype MaxPathLength = MaxPathLength { _pathLength :: Int } deriving stock (Eq, Show)

defaultMaxPathLength :: MaxPathLength
defaultMaxPathLength = MaxPathLength 50