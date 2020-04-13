{-# LANGUAGE DerivingStrategies #-}

module Config 
       (
           -- Data Types
           Config(..)
        ,  Hostname(..)
           -- Functions
        ,  defaultConfig
        ) where


newtype Hostname = Hostname { _hostname :: String } deriving stock (Eq, Show)

data Config = Config { _overrideHostname :: Maybe Hostname } deriving stock (Eq, Show)

defaultConfig :: Config
defaultConfig =
  Config {
    _overrideHostname = Just $ Hostname "mbp"
  }