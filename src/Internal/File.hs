{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies  #-}

module Internal.File
       (
          -- Functions
          getCurrentDirectory
       ,  getUser
       ,  isGitRepo
          -- Data types
       ,  CurrentDirectory(..)
       ,  User(..)
       ) where

import qualified System.Environment as E
import qualified System.Directory   as D

import Internal.Safe (ignoringError)

newtype CurrentDirectory = CurrentDirectory String deriving stock Show 
newtype User = User String deriving stock Show 

getCurrentDirectory  :: IO (Maybe CurrentDirectory)
getCurrentDirectory = ignoringError (CurrentDirectory <$> D.getCurrentDirectory)

getUser :: IO (Maybe User)
getUser = ignoringError (User <$> E.getEnv "USER")

isGitRepo :: IO (Maybe Bool)
isGitRepo = ignoringError $ (<> "/.git") <$> D.getCurrentDirectory >>= D.doesDirectoryExist
