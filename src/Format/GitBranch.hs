{-# LANGUAGE DerivingStrategies #-}

module Format.GitBranch
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

import qualified Parser.GitParser as Git
import qualified Colourista.Pure  as Colour

import Data.List   (isPrefixOf)
import Text.Parsec (parse)

data GitBranchType = LocalAndRemote Git.LocalAndRemoteBranch  | InitialCommit deriving stock (Eq, Show)

formatGitBranch :: Maybe GitBranchType -> String
formatGitBranch (Just InitialCommit)                             = branchColour "InitialCommit"
formatGitBranch (Just (LocalAndRemote (Git.LocalAndRemoteBranch (Git.LocalBranch localBranch) (Just (Git.RemoteBranch remote branch))))) = 
  "[" <> (branchColour localBranch) <> " -> " <> (remoteColour remote) <> "/" <> (branchColour branch) <> "]"
formatGitBranch (Just (LocalAndRemote (Git.LocalAndRemoteBranch (Git.LocalBranch branch) Nothing))) = "[" <> (branchColour branch) <> "]"
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
processModified True  = ":" <> (Colour.formatWith [Colour.cyan] "*" )
processModified False = ""

isModified       :: Maybe [String] -> Bool
isModified       (Just output)
  | any ((0 /=) . length) output = True
  | otherwise                    = False
isModified       Nothing         = False

processGitRepo ::   Maybe [String] -> String
processGitRepo = formatGitBranch . processGitBranch

branchColour :: String -> String
branchColour = Colour.formatWith [Colour.red]

remoteColour :: String -> String
remoteColour = Colour.formatWith [Colour.yellow]