module Lib
      ( 
         prompt
      ) where

import qualified Api as A
import qualified Parser.GitParser as Git
import qualified Colourista.Pure  as Colour

import Text.Parsec (parse)
import Data.List   (isPrefixOf)

data GitBranchType = Local Git.LocalBranch | Remote Git.RemoteBranch | InitialCommit

-- Need to add Colour support
prompt :: IO String
prompt = do
  localTime    <- processTime                        <$> A.getLocalTime
  user         <- processUser                        <$> A.getUser
  path         <- (colourPath . processPath)         <$> A.getCurrentDirectory
  isGitRepo    <- A.isGitRepo
  case isGitRepo of
    Just True -> do
      (branch, modified) <- processGitRepo
      pure (
              localTime <> 
              ":"       <> 
              user      <> 
              "@mbp"    <> 
              ":"       <> 
              path      <>
              ":"       <>
              branch    <>
              modified  <>
              promptStart
            )
    _         -> 
      pure (
              localTime <> 
              ":"       <> 
              user      <> 
              "@mbp"    <> 
              ":"       <> 
              path      <>
              promptStart
        )

processGitRepo :: IO (String, String)
processGitRepo = do
  branch       <- formatGitBranch . processGitBranch <$> A.gitBranchVerbose
  status       <- isModified                         <$> A.gitStatusShort
  pure (branch, processModified $ status)

-- push to a config object
maxPathLength :: Int
maxPathLength = 40

processPath :: Maybe A.CurrentDirectory  -> String
processPath (Just (A.CurrentDirectory pwd)) = 
  let pathLength = length pwd
  in if pathLength > maxPathLength then
    "..." <> takeR (maxPathLength - 3) pwd
  else
    pwd
processPath Nothing = "-"

colourPath :: String -> String
colourPath = Colour.formatWith [Colour.cyan] 

promptStart :: String
promptStart = "> "

takeR :: Int -> [a] -> [a]
takeR n xs 
  | n <= 0    = xs
  | otherwise = reverse . take n . reverse $ xs

processTime :: Maybe A.LocalTime  -> String
processTime (Just (A.LocalTime localTime)) = "[" <> (take 19 localTime) <> "]"
processTime Nothing = "-"

processUser :: Maybe A.User  -> String
processUser (Just (A.User user)) = user
processUser Nothing = "-"

processModified :: Bool -> String
processModified True  = ":" <> (Colour.formatWith [Colour.cyan] "*" )
processModified False = ""

isModified       :: Maybe [String] -> Bool
isModified       (Just output)
  | any ((0 /=) . length) output = True
  | otherwise                    = False
isModified       Nothing         = False

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