module Feature.Timestamp
       (
          -- Functions
          processTimestamp
       ) where

import qualified Api as A
import Config

processTimestamp :: Config -> IO String
processTimestamp config = 
  let localTimeMaybe = case config of
                        Config { _pleatTimestampOption = OptionOn TimestampOption } -> Just . processTime <$> A.getLocalTime
                        Config { _pleatTimestampOption = OptionOff }                -> pure Nothing
  in fmap (maybe "" (<> ":")) localTimeMaybe

processTime :: Maybe A.LocalTime  -> String
processTime (Just (A.LocalTime localTime)) = "[" <> (take 19 localTime) <> "]"
processTime Nothing = "-"
