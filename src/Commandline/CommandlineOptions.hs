{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell    #-}

module Commandline.CommandlineOptions 
      (
         -- Data types
         OptionStatus(..)
         -- Functions
      ,  pleatInfo
      ,  parseConfig
      ,  parseHostnameDisabled
      ,  parseGitDisabled
      ,  parseTimestampDisabled
      ,  parsePathDisabled
      ,  parseOptionStatus
      ,  parseGitOption
      ,  parsePleatHostnameOption
      ,  optionStatusToPleatOption
      ,  parseHostname
      ,  parseMaxPathLength
      ,  parseArguments
      ,  parsePrompt
      ,  versionHelper
      ) where

import Options.Applicative
import Config

import Paths_pleat (version)
import Development.GitRev (gitHash)
import Data.Semigroup ((<>))

import qualified Data.Version as DV

data OptionStatus = Enabled | Disabled deriving stock (Eq, Show)

parseArguments :: IO Config
parseArguments = execParser pleatInfo

pleatInfo :: ParserInfo Config
pleatInfo = 
  info (parseConfig <**> versionHelper <**> helper) (
    fullDesc                                                         <>
    progDesc "writes out a bash prompt with useful information"      <>
    header ("pleat - bash prompt")                                   <>
    footer "--no-feature options take precedence over other options"
  )

parseConfig :: Parser Config
parseConfig = 
  Config <$> 
    parsePleatHostnameOption <*> 
    parsePathOption          <*> 
    parseGitOption           <*> 
    parseTimestampOption     <*>
    parsePrompt

parseHostnameDisabled :: Parser OptionStatus
parseHostnameDisabled = parseOptionStatus "hostname"

parseGitDisabled :: Parser OptionStatus
parseGitDisabled = parseOptionStatus "git"

parseTimestampDisabled :: Parser OptionStatus
parseTimestampDisabled = parseOptionStatus "timestamp"

parsePathDisabled :: Parser OptionStatus
parsePathDisabled = parseOptionStatus "path"

parseOptionStatus :: String -> Parser OptionStatus
parseOptionStatus optionName = 
  flag Enabled Disabled (
    long ("no-" <> optionName) <>
    help ("turn off " <> optionName <> " display")
  )

versionHelper :: Parser (a -> a)
versionHelper = 
  infoOption ("pleat version:" <> DV.showVersion version <>  " githash:" <> $(gitHash))
             (
                short 'v'                 <>
                long "version"            <> 
                help "Show pleat version"
             )

parseGitOption :: Parser (PleatOption GitOption)
parseGitOption = liftA2 optionStatusToPleatOption parseGitDisabled (pure GitOption)

parseTimestampOption :: Parser (PleatOption TimestampOption)
parseTimestampOption = liftA2 optionStatusToPleatOption parseTimestampDisabled (pure TimestampOption)

parsePathOption :: Parser (PleatOption PathOption)
parsePathOption = liftA2 optionStatusToPleatOption parsePathDisabled (PathOption <$> parseMaxPathLength)

parsePleatHostnameOption :: Parser (PleatOption HostnameOption)
parsePleatHostnameOption = liftA2 optionStatusToPleatOption parseHostnameDisabled (HostnameOption <$> parseHostname) 

optionStatusToPleatOption :: OptionStatus -> a -> PleatOption a
optionStatusToPleatOption Enabled  = OptionOn
optionStatusToPleatOption Disabled = const OptionOff

parseHostname :: Parser (Maybe Hostname)
parseHostname = 
  let supplied = 
        Hostname <$> strOption (
          long "hostname"          <>
          help "override hostname" <>
          metavar "HOSTNAME"
        )
  in (Just <$> supplied) <|> (pure Nothing)

parseMaxPathLength :: Parser MaxPathLength
parseMaxPathLength = 
  MaxPathLength <$> 
    option auto (
      long "max-path-length"                       <>
      help "maximum length for the path displayed" <>
      showDefault                                  <>
      value (_pathLength defaultMaxPathLength)     <>
      metavar "INT"
    )

parsePrompt :: Parser Prompt
parsePrompt = 
  Prompt <$>
    strOption (
      long "prompt"                 <>
      help "override prompt"        <>
      showDefault                   <>
      value (_prompt defaultPrompt) <>
      metavar "PROMPT"
    )
