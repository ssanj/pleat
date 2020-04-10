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

data GitBranchType = Local Git.LocalBranch | Remote Git.RemoteBranch | InitialCommit deriving stock (Eq, Show)

formatGitBranch :: Maybe GitBranchType -> String
formatGitBranch (Just (Remote (Git.RemoteBranch remote branch))) = "[" <> (Colour.formatWith [Colour.yellow] remote) <> "|" <> (Colour.formatWith [Colour.red] branch) <> "]"
formatGitBranch (Just (Local (Git.LocalBranch branch)))          = "[" <> (Colour.formatWith [Colour.red] branch) <> "]"
formatGitBranch (Just InitialCommit)                             = Colour.formatWith [Colour.red] "InitialCommit"
formatGitBranch Nothing = "-"

-- TODO: Write a test for this
processGitBranch :: Maybe [String] -> Maybe GitBranchType
processGitBranch (Just linesOfOutput@(_:_)) = 
  let matchedLines =  take 1 $ filter ("*" `isPrefixOf`) linesOfOutput
  in case matchedLines of
      []               -> Nothing
      (branchString:_) ->
        let remoteBranchParseResult = parse Git.remoteBranch "" branchString
            localBranchParseResult  = parse Git.localBranch  "" branchString
        in case (remoteBranchParseResult, localBranchParseResult) of
                (Right (Just b@(Git.RemoteBranch _ _)),           _)                                 -> Just $ Remote b -- "[" <> remote <> "/" <> branch <> "]"
                (_,                                               Right(Just l@(Git.LocalBranch _))) -> Just $ Local l-- "[" <> branch <> "]"
                (_,                                               _)                                 -> Nothing -- "-"
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

