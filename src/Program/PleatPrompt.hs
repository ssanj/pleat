module Program.PleatPrompt
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

import qualified Feature as F

import Control.Applicative ((<|>), liftA2)
import Data.Maybe          (catMaybes)
import Data.List           (intercalate)
import Commandline.CommandlineOptions (PleatCommand(..), versionInfo, versionString)

import Program.Model
import Config

promptBehaviour :: Monad m => F.PromptBehaviour m -> Config -> m String
promptBehaviour behaviour config = do
  localTime           <- F._processTimestamp   behaviour $ config
  user                <- F._processUser        behaviour
  hostname            <- F._processHostname    behaviour $ config
  path                <- F._processPath        behaviour $ config
  gitBranches         <- F._processGitRepo     behaviour $ config
  let promptSuffix    = F._processPromptSuffix behaviour $ config
      loginAtMachine  = mkLoginAtMachine user hostname
      promptSeparator = F._promptSeparator . F._processPromptSeparator behaviour $ config
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
      behaviour :: F.PromptBehaviour IO
      behaviour = F.PromptBehaviour
                    F.processTimestamp
                    F.processUser
                    F.processHostname
                    F.processPath
                    F.processGitRepo
                    F.processPromptSuffix
                    F.processPromptSeparator


mkLoginAtMachine :: Maybe F.User -> Maybe F.Hostname -> Maybe Promptable
mkLoginAtMachine user hostname = (liftA2 LoginAtMachine user hostname) <|> (Login <$> user) <|> (Machine <$> hostname)

combinePromptables :: (a -> String) -> String -> [Maybe a] -> String
combinePromptables toString sep = intercalate sep . fmap toString . catMaybes

