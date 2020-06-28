{-# LANGUAGE DerivingStrategies #-}

module Feature.Live.Format.GitBranch
       (
          -- Data Types
          GitBranchType(..)
          -- Functions
       ,  processGitRepo
       ,  processModified
       ,  isModified
          -- Internal
       ,  processGitBranch
       ,  formatGitBranch
       ) where

import qualified Feature.Live.Parser               as Git
import qualified Feature.Live.Internal.BashColours as Colour

import Data.List   (isPrefixOf)
import Text.Parsec (parse)

data GitBranchType = LocalAndRemote Git.LocalAndRemoteBranch  | InitialCommit deriving stock (Eq, Show)

formatGitBranch :: Maybe GitBranchType -> String
formatGitBranch (Just InitialCommit)                             = branchColour "InitialCommit"
formatGitBranch (Just (LocalAndRemote (Git.LocalAndRemoteBranch (Git.LocalBranch localBranch (Git.GitHash hash)) (Just (Git.RemoteBranch remote branch (Just (Git.CommitsAhead commitsAhead))))))) =
  "["                                    <>
  (branchColour localBranch)             <>
  "(" <> hash <> ")"                     <>
  " -> "                                 <>
  (remoteColour remote)                  <>
  "/"                                    <>
  (branchColour branch)                  <>
  "(^"                                   <>
  (modifiedColour . show $ commitsAhead) <>
  ")]"
formatGitBranch (Just (LocalAndRemote (Git.LocalAndRemoteBranch (Git.LocalBranch localBranch (Git.GitHash hash)) (Just (Git.RemoteBranch remote branch Nothing))))) =
  "["                        <>
  (branchColour localBranch) <>
  "(" <> hash <> ")"         <>
  " -> "                     <>
  (remoteColour remote)      <>
  "/"                        <>
  (branchColour branch)      <>
  "]"
formatGitBranch (Just (LocalAndRemote (Git.LocalAndRemoteBranch (Git.LocalBranch branch (Git.GitHash hash)) Nothing))) =
  "["                   <>
  (branchColour branch) <>
  "(" <> hash <> ")"    <>
  "]"
formatGitBranch Nothing = "-"

-- TODO: Write a test for this
processGitBranch :: Maybe [String] -> Maybe GitBranchType
processGitBranch (Just linesOfOutput@(_:_)) =
  let matchedLines =  take 1 $ filter ("*" `isPrefixOf`) linesOfOutput
  in case matchedLines of
      []               -> Nothing -- No active branch
      (branchString:_) ->
        let localAndRemote = parse Git.localAndRemoteBranch "" branchString
        in case localAndRemote of
          Right (Just lrb@(Git.LocalAndRemoteBranch _ _)) -> Just $ LocalAndRemote lrb
          _                                               -> Nothing -- Either we got an error or we have no local branch - which is also an error
processGitBranch (Just [])              = Just InitialCommit -- This means that we got no lines of input for branches which indicates that it's an initial branch
processGitBranch Nothing                = Nothing

processModified :: Bool -> String
processModified True  = ":" <> (modifiedColour "*" )
processModified False = ""

isModified       :: Maybe [String] -> Bool
isModified       (Just output)
  | any ((0 /=) . length) output = True
  | otherwise                    = False
isModified       Nothing         = False

processGitRepo ::   Maybe [String] -> String
processGitRepo = formatGitBranch . processGitBranch

branchColour :: String -> String
branchColour = Colour.red

remoteColour :: String -> String
remoteColour = Colour.yellow

modifiedColour :: String -> String
modifiedColour = Colour.cyan