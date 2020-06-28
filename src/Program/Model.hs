{-# LANGUAGE DerivingStrategies #-}

module Program.Model
       (
          -- Data types
          Promptable(..)

          -- Functions
       ,  showPromptable
       ) where

import qualified Feature as F

data Promptable = LocalTime F.DateTime
                | Login F.User
                | Machine F.Hostname
                | LoginAtMachine F.User F.Hostname
                | CWD F.Path
                | GitInfo F.GitBranchModification
                | PromptSuffix F.Prompt  deriving stock (Eq, Show)

showPromptable :: Promptable -> String
showPromptable (LocalTime (F.DateTime dateTime))                    = dateTime
showPromptable (Login (F.User user))                                = user
showPromptable (Machine (F.Hostname hostname))                      = hostname
showPromptable (LoginAtMachine (F.User user) (F.Hostname hostname)) = (user <> "@" <> hostname)
showPromptable (CWD (F.Path path))                                  = path
showPromptable (GitInfo (F.GitBranchModification branch modified))  = (branch <> modified)
showPromptable (PromptSuffix (F.Prompt suffix))                     = suffix
