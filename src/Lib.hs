module Lib
      ( 
         prompt
      ) where

import qualified Api as A
import qualified Parser.GitParser as Git

import Text.Parsec (parse)

-- Need to add Colour support
prompt :: IO String
prompt = do
  localTime    <- processTime      <$> A.getLocalTime
  user         <- processUser      <$> A.getUser
  path         <- processPath      <$> A.getCurrentDirectory
  branch       <- processGitBranch <$> A.gitBranchVerbose
  status       <- isModified       <$> A.gitStatusShort
  let modified = processModified status
  pure (
          localTime <> 
          ":"       <> 
          user      <> 
          "@mbp"    <> 
          ":"       <> 
          path      <>
          ":"       <>
          branch    <>
          modified
        )

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
processModified True  = ":" <> "*" 
processModified False = ""

isModified       :: Maybe [String] -> Bool
isModified       (Just output)
  | any ((0 /=) . length) output = True
  | otherwise                    = False
isModified       Nothing         = False

processGitBranch :: Maybe [String] -> String
processGitBranch (Just (branchString:_)) = 
  let remoteBranchParseResult = parse Git.remoteBranch "" branchString
      localBranchParseResult  = parse Git.localBranch  "" branchString
  in case (remoteBranchParseResult, localBranchParseResult) of
       (Right (Just (Git.RemoteBranch remote branch)), _)                                    -> "[" <> remote <> "/" <> branch <> "]"
       (_,                                             Right(Just (Git.LocalBranch branch))) -> "[" <> branch <> "]"
       (_,                                             _)                                    -> "-"
processGitBranch (Just [])              = "-"
processGitBranch Nothing                = "-"