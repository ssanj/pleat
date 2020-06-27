{-# LANGUAGE DerivingStrategies #-}

module Program
       (
          -- Data types
          Promptable(..)
          -- Functions
       ,  prompt
       ,  promptBehaviour
       ,  showPromptable
       ,  mkLoginAtMachine
       ,  combinePromptables
       ) where

import qualified Feature.Git             as F
import qualified Feature.Timestamp       as F
import qualified Feature.Hostname        as F
import qualified Feature.Path            as F
import qualified Feature.User            as F
import qualified Feature.Prompt          as F
import qualified Feature.PromptSeparator as F
import Feature.Model (PromptBehaviour(..))

import Control.Applicative ((<|>), liftA2)
import Data.Maybe          (catMaybes)
import Data.List           (intercalate)
import Commandline.CommandlineOptions (PleatCommand(..), versionInfo, versionString)

import Config


-- TODO: extract prompt section separator - don't default to ":"
promptBehaviour :: Monad m => PromptBehaviour m -> Config -> m String
promptBehaviour behaviour config = do
  localTime           <- _processTimestamp   behaviour $ config
  user                <- _processUser        behaviour
  hostname            <- _processHostname    behaviour $ config
  path                <- _processPath        behaviour $ config
  gitBranches         <- _processGitRepo     behaviour $ config
  let promptSuffix    = _processPromptSuffix behaviour $ config
      loginAtMachine  = mkLoginAtMachine user hostname
      promptSeparator = F._promptSeparator . _processPromptSeparator behaviour $ config
      fullPrompt      =
        combinePromptables showPromptable promptSeparator [
                                                             LocalTime <$> localTime
                                                          ,  loginAtMachine
                                                          ,  CWD <$> path
                                                          ,  GitInfo <$> gitBranches
                                                          ,  PromptSuffix <$> promptSuffix
                                                          ]
  pure fullPrompt

prompt :: PleatCommand -> IO String
prompt PleatVersionCommand = pure $ versionString versionInfo
prompt (PleatConfigCommand config) =
  promptBehaviour behaviour config
    where
      behaviour :: PromptBehaviour IO
      behaviour = PromptBehaviour
                    F.processTimestamp
                    F.processUser
                    F.processHostname
                    F.processPath
                    F.processGitRepo
                    F.processPromptSuffix
                    F.processPromptSeparator

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

mkLoginAtMachine :: Maybe F.User -> Maybe F.Hostname -> Maybe Promptable
mkLoginAtMachine user hostname = (liftA2 LoginAtMachine user hostname) <|> (Login <$> user) <|> (Machine <$> hostname)

combinePromptables :: (a -> String) -> String -> [Maybe a] -> String
combinePromptables toString sep = intercalate sep . fmap toString . catMaybes

