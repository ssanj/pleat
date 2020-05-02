module Feature.User
       (
          -- Functions
          processUser
       ) where

import qualified Api as A

processUser :: IO String
processUser = username <$> A.getUser

username :: Maybe A.User  -> String
username (Just (A.User user)) = user
username Nothing = "-"
