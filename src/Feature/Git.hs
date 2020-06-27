{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}

module Feature.Git
       (
          -- Functions
          processGitRepo
       ) where

import qualified Api as A
import qualified Format.GitBranch as GF

import Config
import Feature.Model (GitBranchModification(..))

processGitRepo :: Config -> IO (Maybe GitBranchModification)
processGitRepo Config {_pleatGitOption = OptionOn GitOption } = do
  maybeBool <- A.isGitRepo
  case maybeBool of
    Just True  -> Just <$> gitBranchAndModification
    _          -> pure Nothing
processGitRepo Config {_pleatGitOption = OptionOff }          = pure Nothing

gitBranchAndModification :: IO GitBranchModification
gitBranchAndModification = do
  branch       <- GF.processGitRepo <$> A.gitBranchVerbose
  status       <- GF.isModified     <$> A.gitStatusShort
  pure $ GitBranchModification branch (GF.processModified status)