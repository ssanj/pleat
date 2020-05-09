{-# LANGUAGE DerivingStrategies #-}

module Lib
       ( 
          -- Data types
          Promptable(..)
          -- Functions        
       ,  prompt
       ,  showPromptable
       ,  mkLoginAtMachine
       ,  combinePromptables
       ) where

import qualified Feature.Git       as F
import qualified Feature.Timestamp as F
import qualified Feature.Hostname  as F
import qualified Feature.Path      as F
import qualified Feature.User      as F
import qualified Feature.Prompt    as F
        
import Control.Applicative ((<|>), liftA2)
import Data.Maybe          (catMaybes)
import Data.List           (intercalate)

import Config

prompt :: Config -> IO String
prompt config = do
  localTime          <- F.processTimestamp config
  user               <- F.processUser
  hostname           <- F.processHostname config
  path               <- F.processPath config
  gitBranches        <- F.processGitRepo config
  let promptSuffix   = F.processPromptSuffix config
      loginAtMachine = mkLoginAtMachine user hostname
      fullPrompt     = combinePromptables showPromptable ":" [
                                                                LocalTime <$> localTime
                                                             ,  loginAtMachine
                                                             ,  CWD <$> path
                                                             ,  GitInfo <$> gitBranches
                                                             ,  PromptSuffix <$> promptSuffix
                                                             ]
  pure fullPrompt

data Promptable = LocalTime F.DateTime 
                | Login F.User
                | Machine F.Hostname
                | LoginAtMachine F.User F.Hostname
                | CWD F.Path
                | GitInfo F.GitBranchModification
                | PromptSuffix F.Prompt deriving stock (Eq, Show)

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

combinePromptables :: (Promptable -> String) -> String -> [Maybe Promptable] -> String
combinePromptables toString sep = intercalate sep . fmap toString . catMaybes

