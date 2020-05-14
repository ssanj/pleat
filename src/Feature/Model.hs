{-# LANGUAGE KindSignatures #-}

module Feature.Model
       (
          -- Data types
          PromptBehaviour(..)
       ) where

import Data.Kind (Type)

import qualified Feature.Git       as F
import qualified Feature.Timestamp as F
import qualified Feature.Hostname  as F
import qualified Feature.Path      as F
import qualified Feature.User      as F
import qualified Feature.Prompt    as F

import Config

data PromptBehaviour (m :: Type -> Type) =
  PromptBehaviour {
    _processTimestamp    :: Config -> m (Maybe F.DateTime)
  , _processUser         :: m (Maybe F.User)
  , _processHostname     :: Config -> m (Maybe F.Hostname)
  , _processPath         :: Config -> m (Maybe F.Path)
  , _processGitRepo      :: Config -> m (Maybe F.GitBranchModification)
  , _processPromptSuffix :: Config -> Maybe F.Prompt
  }