{-# LANGUAGE DerivingStrategies  #-}

module Internal.Time
       (
          -- Functions
          getLocalTime
          -- Data types
       ,  LocalTime(..)
       ) where

import Internal.Safe (ignoringError)

import qualified Data.Thyme as T
import qualified Data.Thyme.Time.Core as T

newtype LocalTime = LocalTime String deriving stock Show 

getLocalTime :: IO (Maybe LocalTime)
getLocalTime = 
  let localTime = T.utcToLocalTime <$> T.getCurrentTimeZone <*> T.getCurrentTime
  in ignoringError $ LocalTime . show <$> localTime
