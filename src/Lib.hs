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
  localTime  <- processTimestamp config
  user       <- processUser            <$> A.getUser
  hostname   <- processHostname config
  path       <- (processPath config)   <$> A.getCurrentDirectory
  isGitRepo  <- (enableGitRepo config) <$> A.isGitRepo
  case isGitRepo of
    True -> do
      (branch, modified) <- processGitRepo
      pure (
              localTime <>
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
              user      <> 
              hostname  <> 
              ":"       <> 
              path      <>
              promptStart
        )

processPath :: Config -> (Maybe A.CurrentDirectory) -> String
processPath = PF.processPath . _maxPathLength

processTimestamp :: Config -> IO String
processTimestamp config = 
  let localTimeMaybe = case config of
                        Config { _pleatTimestampOption = OptionOn TimestampOption } -> Just . processTime <$> A.getLocalTime
                        Config { _pleatTimestampOption = OptionOff }                -> pure Nothing
  in fmap (maybe "" (<> ":")) localTimeMaybe

processHostname :: Config -> IO String
processHostname config =
  let hostnameMaybe = case config of
                        Config { _pleatHostnameOption = OptionOn (HostnameOption (Just (Hostname hostnameOverride))) } -> pure $ Just hostnameOverride
                        Config { _pleatHostnameOption = OptionOn (HostnameOption Nothing) }                            -> Just . _hostname <$> A.getHostname
                        Config { _pleatHostnameOption = OptionOff }                                                    -> pure Nothing
  in fmap (maybe "" ("@" <> )) hostnameMaybe

enableGitRepo :: Config -> (Maybe Bool) -> Bool
enableGitRepo Config {_pleatGitOption = OptionOn GitOption } hasGitDir = maybe False id hasGitDir
enableGitRepo Config {_pleatGitOption = OptionOff }                  _ = False


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
