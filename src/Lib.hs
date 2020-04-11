module Lib
      ( 
         prompt
      ) where

import qualified Api as A
import qualified Format.GitBranch as GF
import qualified Format.Path      as PF

-- Need to add Colour support
prompt :: IO String
prompt = do
  localTime    <- processTime    <$> A.getLocalTime
  user         <- processUser    <$> A.getUser
  path         <- PF.processPath <$> A.getCurrentDirectory
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
  branch       <- GF.processGitRepo <$> A.gitBranchVerbose
  status       <- GF.isModified     <$> A.gitStatusShort
  pure (branch, GF.processModified $ status)

promptStart :: String
promptStart = "> "

processTime :: Maybe A.LocalTime  -> String
processTime (Just (A.LocalTime localTime)) = "[" <> (take 19 localTime) <> "]"
processTime Nothing = "-"

processUser :: Maybe A.User  -> String
processUser (Just (A.User user)) = user
processUser Nothing = "-"
