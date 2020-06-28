{-# LANGUAGE DerivingStrategies #-}

module Feature.Live.Hostname
       (
          -- Functions
          processHostname
       ) where

import qualified Feature.Live.Internal as A

import Config (Config(..), HostnameOption(..), PleatOption(..))
import qualified Config as C
import Feature.Model (Hostname(..))

processHostname :: Config -> IO (Maybe Hostname)
processHostname Config { _pleatHostnameOverrideOption = OptionOn (HostnameOption (Just (C.Hostname hostnameOverride))) } =
  pure $ Just $ Hostname hostnameOverride
processHostname Config { _pleatHostnameOverrideOption = OptionOn (HostnameOption Nothing) }                              =
  (\(C.Hostname hostname) -> Just $ Hostname hostname) <$> A.getHostname
processHostname Config { _pleatHostnameOverrideOption = OptionOff }                                                      =
  pure Nothing

