module Feature.Git
       (
          -- Functions
          isEnableGitRepo
       ,  processGitRepo
       ) where

import qualified Api as A
import qualified Format.GitBranch as GF

import Config

isEnableGitRepo :: Config -> IO Bool
isEnableGitRepo Config {_pleatGitOption = OptionOn GitOption } = (maybe False id) <$> A.isGitRepo
isEnableGitRepo Config {_pleatGitOption = OptionOff }          = pure False

processGitRepo :: IO (String, String)
processGitRepo = do
  branch       <- GF.processGitRepo <$> A.gitBranchVerbose
  status       <- GF.isModified     <$> A.gitStatusShort
  pure (branch, GF.processModified $ status)