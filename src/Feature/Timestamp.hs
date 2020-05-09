{-# LANGUAGE DerivingStrategies #-}

module Feature.Timestamp
       (
          -- Data types
          DateTime(..)
          -- Functions
       ,  processTimestamp
       ) where

import qualified Api as A
import Config

newtype DateTime = DateTime { _dateTime :: String } deriving stock (Eq, Show)

processTimestamp :: Config -> IO (Maybe DateTime)
processTimestamp Config { _pleatTimestampOption = OptionOn TimestampOption } = fmap processTime <$> A.getLocalTime
processTimestamp Config { _pleatTimestampOption = OptionOff }                = pure Nothing

processTime :: A.LocalTime  -> DateTime
processTime (A.LocalTime localTime) = DateTime $ "[" <> (take 19 localTime) <> "]"
