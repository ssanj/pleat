{-# LANGUAGE DerivingStrategies #-}

module Feature.User
       (
          -- Data types
          User(..)
          -- Functions
       ,  processUser
       ) where

import qualified Api as A

newtype User = User { _user :: String } deriving stock (Eq, Show)

processUser :: IO (Maybe User)
processUser =
  fmap (\(A.User user)-> User user) <$> A.getUser
