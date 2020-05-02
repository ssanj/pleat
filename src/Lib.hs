module Lib
       ( 
          prompt
       ) where

import qualified Feature.Git       as F
import qualified Feature.Timestamp as F
import qualified Feature.Hostname  as F
import qualified Feature.Path      as F
import qualified Feature.User      as F
import qualified Feature.Prompt    as F

import Config

prompt :: Config -> IO String
prompt config = do
  localTime       <- F.processTimestamp config
  user            <- F.processUser
  hostname        <- F.processHostname config
  path            <- F.processPath config
  showGitFeatures <- F.isEnableGitRepo config
  let promptEnd   = F.processPromptSuffix config
  case showGitFeatures of
    True -> do
      (branch, modified) <- F.processGitRepo
      pure (
              localTime <>
              user      <> 
              hostname  <>
              separator <> 
              path      <>
              separator <> 
              branch    <>
              modified  <>
              promptEnd
            )
    False -> 
      pure (
              localTime <>
              user      <>
              hostname  <>
              separator <>
              path      <>
              promptEnd
        )

separator :: String
separator = ":"
