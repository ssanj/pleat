module Feature.Hostname
       (
          -- Functions
          processHostname
       ) where

import qualified Api as A

import Config


processHostname :: Config -> IO String
processHostname config =
  let hostnameMaybe = case config of
                        Config { _pleatHostnameOption = OptionOn (HostnameOption (Just (Hostname hostnameOverride))) } -> pure $ Just hostnameOverride
                        Config { _pleatHostnameOption = OptionOn (HostnameOption Nothing) }                            -> Just . _hostname <$> A.getHostname
                        Config { _pleatHostnameOption = OptionOff }                                                    -> pure Nothing
  in fmap (maybe "" ("@" <> )) hostnameMaybe
