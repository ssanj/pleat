{-# LANGUAGE DerivingStrategies #-}

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
      ,  parseOptionStatus
      ,  parseGitOption
      ,  parsePleatHostnameOption
      ,  optionStatusToPleatOption
      ,  parseHostname
      ,  parseMaxPathLength
      ,  parseArguments
      ) where

import Options.Applicative
import Config

import Data.Semigroup ((<>))

data OptionStatus = Enabled | Disabled deriving stock (Eq, Show)

parseArguments :: IO Config
parseArguments = execParser pleatInfo

pleatInfo :: ParserInfo Config
pleatInfo = 
  info (parseConfig <**> helper) (
    fullDesc <>
    progDesc "writes out a bash prompt with useful information" <>
    header "pleat - bash prompt" <>
    footer "--no options take precedence over other options"
  )

parseConfig :: Parser Config
parseConfig = 
  Config <$> parsePleatHostnameOption <*> parseMaxPathLength <*> parseGitOption <*> parseTimestampOption

parseHostnameDisabled :: Parser OptionStatus
parseHostnameDisabled = parseOptionStatus "hostname"

parseGitDisabled :: Parser OptionStatus
parseGitDisabled = parseOptionStatus "git"

parseTimestampDisabled :: Parser OptionStatus
parseTimestampDisabled = parseOptionStatus "timestamp"

parseOptionStatus :: String -> Parser OptionStatus
parseOptionStatus optionName = 
  flag Enabled Disabled (
    long ("no-" <> optionName) <>
    help ("turn off " <> optionName <> " display")
  )

parseGitOption :: Parser (PleatOption GitOption)
parseGitOption = liftA2 optionStatusToPleatOption parseGitDisabled (pure GitOption)

parseTimestampOption :: Parser (PleatOption TimestampOption)
parseTimestampOption = liftA2 optionStatusToPleatOption parseTimestampDisabled (pure TimestampOption)

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
