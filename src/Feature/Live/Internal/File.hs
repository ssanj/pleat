{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies  #-}

module Feature.Live.Internal.File
       (
          -- Functions
          getCurrentDirectory
       ,  getUser
       ,  isGitRepo
       ,  getHostname
          -- Data types
       ,  CurrentDirectory(..)
       ,  User(..)
       ) where

import qualified System.Environment as E
import qualified System.Directory   as D
import qualified Network.HostName   as N

import Feature.Live.Internal.Safe (ignoringError)
import Config                     (Hostname(..))

newtype CurrentDirectory = CurrentDirectory String deriving stock Show
newtype User = User String deriving stock Show

getCurrentDirectory  :: IO (Maybe CurrentDirectory)
getCurrentDirectory = ignoringError (CurrentDirectory <$> D.getCurrentDirectory)

getUser :: IO (Maybe User)
getUser = ignoringError (User <$> E.getEnv "USER")

getHostname :: IO Hostname
getHostname = Hostname <$> N.getHostName

isGitRepo :: IO (Maybe Bool)
isGitRepo = ignoringError $ (<> "/.git") <$> D.getCurrentDirectory >>= D.doesDirectoryExist
