module Lib
      ( 
         prompt
      ) where

import qualified Api as A
import qualified Format.GitBranch as GF
import qualified Format.Path      as PF

import Config

prompt :: Config -> IO String
prompt config = do
  localTime     <- processTime      <$> A.getLocalTime
  user          <- processUser      <$> A.getUser
  hostnameMaybe <- processHostname config -- (processHostname <$> A.getHostname) <*> pure (_overrideHostname config)
  let hostname  = maybe "" ("@" <> ) hostnameMaybe
  path          <- (processPath config)   <$> A.getCurrentDirectory
  isGitRepo     <- A.isGitRepo
  case isGitRepo of
    Just True -> do
      (branch, modified) <- processGitRepo
      pure (
              localTime <> 
              ":"       <> 
              user      <> 
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
processPath = PF.processPath . _maxPathLength

processHostname :: Config -> IO (Maybe String)
processHostname Config {_pleatHostnameOption = OptionOn (HostnameOption (Just (Hostname hostnameOverride))), _maxPathLength = _} = pure $ Just hostnameOverride
processHostname Config {_pleatHostnameOption = OptionOn (HostnameOption Nothing),  _maxPathLength = _}                           = Just . _hostname <$> A.getHostname
processHostname Config {_pleatHostnameOption = OptionOff, _maxPathLength =_}                                                     = pure Nothing

-- processHostname :: Hostname -> Maybe Hostname -> String
-- processHostname actualHostname = maybe (_hostname actualHostname) _hostname 

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
