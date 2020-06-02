{-# LANGUAGE DerivingStrategies #-}

module Feature.Hostname
       (
          -- Data types
          Hostname(..)
          -- Functions
       ,  processHostname
       ) where

import qualified Api as A

import Config (Config(..), HostnameOption(..), PleatOption(..))
import qualified Config as C

newtype Hostname = Hostname { _hostname :: String } deriving stock (Eq, Show)

processHostname :: Config -> IO (Maybe Hostname)
processHostname Config { _pleatHostnameOverrideOption = OptionOn (HostnameOption (Just (C.Hostname hostnameOverride))) } =
  pure $ Just $ Hostname hostnameOverride
processHostname Config { _pleatHostnameOverrideOption = OptionOn (HostnameOption Nothing) }                              =
  (\(C.Hostname hostname) -> Just $ Hostname hostname) <$> A.getHostname
processHostname Config { _pleatHostnameOverrideOption = OptionOff }                                                      =
  pure Nothing

