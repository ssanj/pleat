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
        
import Data.String         (IsString (..))
import Data.Maybe          (catMaybes)
import Data.List           (intercalate)

import Config

prompt :: Config -> IO String
prompt config = do
  localTime       <- F.processTimestamp config
  user            <- F.processUser
  hostname        <- F.processHostname config
  path            <- F.processPath config
  gitBranches     <- F.processGitRepo config
  let promptEnd   = F.processPromptSuffix config
      fullPrompt  = combinePrompts ":" [
                                          dateTimeAsPrompt    <$> localTime
                                       ,  maybeUserHost user hostname
                                       ,  pathAsPrompt        <$> path 
                                       ,  gitBranchesAsPrompt <$> gitBranches
                                       ,  promptAsPrompt      <$> promptEnd
                                       ]
  pure fullPrompt
  --pure $ createPrompt localTime user hostname path gitBranches promptEnd
  -- case showGitFeatures of
  --   True -> do
  --     (branch, modified) <- F.processGitRepo
  --     pure (
  --             localTime <>
  --             user      <> 
  --             hostname  <>
  --             separator <> 
  --             path      <>
  --             separator <> 
  --             branch    <>
  --             modified  <>
  --             promptEnd
  --           )
  --   False -> 
  --     pure (
  --             localTime <>
  --             user      <>
  --             hostname  <>
  --             separator <>
  --             path      <>
  --             promptEnd
  --       )

maybeUserHost :: Maybe F.User -> Maybe F.Hostname -> Maybe String
maybeUserHost (Just user) (Just hostname) = Just $ (userAsPrompt user) <> "@" <> (hostnameAsPrompt hostname)
maybeUserHost (Just user) Nothing         = Just $ userAsPrompt user
maybeUserHost Nothing (Just hostname)     = Just $ hostnameAsPrompt hostname
maybeUserHost Nothing Nothing             = Nothing

dateTimeAsPrompt :: IsString a => F.DateTime -> a
dateTimeAsPrompt = fromString . F._dateTime

userAsPrompt :: IsString a => F.User -> a
userAsPrompt = fromString . F._user

hostnameAsPrompt :: IsString a => F.Hostname -> a
hostnameAsPrompt = fromString . F._hostname

pathAsPrompt :: IsString a => F.Path -> a
pathAsPrompt = fromString . F._path

gitBranchesAsPrompt :: (IsString a) => F.GitBranchModification -> a
gitBranchesAsPrompt (F.GitBranchModification branch modified) = fromString (branch <> modified)

promptAsPrompt :: IsString a => F.Prompt -> a
promptAsPrompt = fromString . F._prompt

-- How can we generalise over intercalate?
combinePrompts :: String -> [Maybe String] -> String
combinePrompts sep = intercalate sep . catMaybes
