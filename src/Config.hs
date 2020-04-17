{-# LANGUAGE DerivingStrategies #-}

module Config 
       (
           -- Data Types
           Config(..)
        ,  Hostname(..)
        ,  MaxPathLength(..)
           -- Functions
        ,  defaultConfig
        ,  defaultMaxPathLength
        ) where


newtype Hostname = Hostname { _hostname :: String } deriving stock (Eq, Show)

data Config = Config { 
   _overrideHostname :: Maybe Hostname 
,  _maxPathLength :: MaxPathLength 
} deriving stock (Eq, Show)

newtype MaxPathLength = MaxPathLength { _pathLength :: Int } deriving stock (Eq, Show)

defaultConfig :: Config
defaultConfig =
  Config {
     _overrideHostname = Just $ Hostname "mbp"
  ,  _maxPathLength = defaultMaxPathLength
  }

defaultMaxPathLength :: MaxPathLength
defaultMaxPathLength = MaxPathLength 50