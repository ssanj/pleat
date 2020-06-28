{-# LANGUAGE DerivingStrategies #-}

module Program.Model
       (
          -- Data types
          Promptable(..)
       ) where

import qualified Feature.Feature as F

data Promptable = LocalTime F.DateTime
                | Login F.User
                | Machine F.Hostname
                | LoginAtMachine F.User F.Hostname
                | CWD F.Path
                | GitInfo F.GitBranchModification
                | PromptSuffix F.Prompt  deriving stock (Eq, Show)

