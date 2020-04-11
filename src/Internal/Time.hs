{-# LANGUAGE DerivingStrategies  #-}

module Internal.Time
       (
          -- Functions
          getLocalTime
          -- Data types
       ,  LocalTime(..)
       ) where

import qualified Data.Thyme           as T
import qualified Data.Thyme.Time.Core as T

import Internal.Safe (ignoringError)
import System.Locale (defaultTimeLocale)

newtype LocalTime = LocalTime String deriving stock Show 

getLocalTime :: IO (Maybe LocalTime)
getLocalTime = 
  let localTime = T.utcToLocalTime <$> T.getCurrentTimeZone <*> T.getCurrentTime
  in ignoringError $ LocalTime . localTimeString <$> localTime

localTimeString :: T.LocalTime -> String
localTimeString = T.formatTime defaultTimeLocale "%d/%m/%Y %T"
