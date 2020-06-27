{-# LANGUAGE DerivingStrategies #-}

module Feature.User
       (
          -- Functions
          processUser
       ) where

import qualified Api as A
import Feature.Model (User(..))

processUser :: IO (Maybe User)
processUser =
  fmap (\(A.User user)-> User user) <$> A.getUser
