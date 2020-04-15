module Lib
      ( 
         prompt
      ) where

import qualified Api as A
import qualified Format.GitBranch as GF
import qualified Format.Path      as PF

import Config              (Config(..), Hostname(..), defaultMaxPathLength)

prompt :: Config -> IO String
prompt config = do
  localTime    <- processTime      <$> A.getLocalTime
  user         <- processUser      <$> A.getUser
  hostname     <- (processHostname <$> A.getHostname) <*> pure (_overrideHostname config)
  path         <- (processPath config)   <$> A.getCurrentDirectory
  isGitRepo    <- A.isGitRepo
  case isGitRepo of
    Just True -> do
      (branch, modified) <- processGitRepo
      pure (
              localTime <> 
              ":"       <> 
              user      <> 
              "@"       <> 
              hostname  <> 
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
              hostname  <> 
              ":"       <> 
              path      <>
              promptStart
        )

processPath :: Config -> (Maybe A.CurrentDirectory) -> String
processPath config = PF.processPath (maybe defaultMaxPathLength id (_maxPathLength config))

processHostname :: Hostname -> Maybe Hostname -> String
processHostname actualHostname = maybe (_hostname actualHostname) _hostname 

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
