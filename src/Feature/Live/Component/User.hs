{-# LANGUAGE DerivingStrategies #-}

module Feature.Live.Component.User
       (
          -- Functions
          processUser
       ) where

import qualified Feature.Live.Internal as A
import qualified Data.Text             as T

import Feature.Model (User(..))

processUser :: IO (Maybe User)
processUser =
  fmap (\(A.User user)-> User . T.pack $ user) <$> A.getUser
