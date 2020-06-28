{-# LANGUAGE DerivingStrategies #-}

module Feature.Live.User
       (
          -- Functions
          processUser
       ) where

import qualified Feature.Live.Internal as A
import Feature.Model (User(..))

processUser :: IO (Maybe User)
processUser =
  fmap (\(A.User user)-> User user) <$> A.getUser
