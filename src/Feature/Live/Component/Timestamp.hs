{-# LANGUAGE DerivingStrategies #-}

module Feature.Live.Component.Timestamp
       (
          -- Functions
          processTimestamp
       ) where

import qualified Feature.Live.Internal as A
import qualified Data.Text             as T

import Config
import Feature.Model (DateTime(..))

processTimestamp :: Config -> IO (Maybe DateTime)
processTimestamp Config { _pleatTimestampOption = OptionOn TimestampOption } = fmap processTime <$> A.getLocalTime
processTimestamp Config { _pleatTimestampOption = OptionOff }                = pure Nothing

processTime :: A.LocalTime  -> DateTime
processTime (A.LocalTime localTime) = DateTime . T.pack $ "[" <> (take 19 localTime) <> "]"
