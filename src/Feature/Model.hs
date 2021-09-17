{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DerivingStrategies #-}

module Feature.Model
       (
          -- Data types
          PromptBehaviour(..)
       ,  GitBranchModification(..)
       ,  Hostname(..)
       ,  Path(..)
       ,  Prompt(..)
       ,  PromptSeparator(..)
       ,  DateTime(..)
       ,  User(..)
       ) where

import Data.Kind (Type)
import Config    (Config)

import qualified Data.Text as T

data GitBranchModification =
  GitBranchModification { _gitBranch :: T.Text, _gitModification :: T.Text } deriving stock (Eq, Show)

newtype Hostname = Hostname { _hostname :: T.Text } deriving stock (Eq, Show)

newtype Path = Path { _path :: T.Text } deriving stock (Eq, Show)

newtype Prompt = Prompt { _prompt :: T.Text } deriving stock (Eq, Show)

newtype PromptSeparator = PromptSeparator { _promptSeparator :: T.Text } deriving stock (Eq, Show)

newtype DateTime = DateTime { _dateTime :: T.Text } deriving stock (Eq, Show)

newtype User = User { _user :: T.Text } deriving stock (Eq, Show)

data PromptBehaviour (m :: Type -> Type) =
  PromptBehaviour {
    _processTimestamp       :: Config -> m (Maybe DateTime)
  , _processUser            :: m (Maybe User)
  , _processHostname        :: Config -> m (Maybe Hostname)
  , _processPath            :: Config -> m (Maybe Path)
  , _processGitRepo         :: Config -> m (Maybe GitBranchModification)
  , _processPromptSuffix    :: Config -> Maybe Prompt
  , _processPromptSeparator :: Config -> PromptSeparator
  }