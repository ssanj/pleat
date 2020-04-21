module Commandline.CommandlineOptions 
      (
         -- Functions
         pleatInfo
      ,  parseConfig
      ,  parseHostnameDisabled
      ,  parseGitDisabled
      ,  parseBooleanOption
      ,  parseGitOption
      ,  parsePleatHostnameOption
      ,  handlePleatDisableOption
      ,  parseHostname
      ,  parseMaxPathLength
      ,  parseArguments
      ) where

import Options.Applicative
import Config

import Data.Semigroup ((<>))

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
  Config <$> parsePleatHostnameOption <*> parseMaxPathLength <*> parseGitOption

parseHostnameDisabled :: Parser Bool
parseHostnameDisabled = parseBooleanOption "hostname"

parseGitDisabled :: Parser Bool
parseGitDisabled = parseBooleanOption "git"

-- TODO: Boolean blindness
parseBooleanOption :: String -> Parser Bool
parseBooleanOption optionName = 
  flag False True (
    long ("no-" <> optionName) <>
    help ("turn off " <> optionName <> " display")
  )

parseGitOption :: Parser (PleatOption GitOption)
parseGitOption = liftA2 handlePleatDisableOption parseGitDisabled (pure GitOption)

parsePleatHostnameOption :: Parser (PleatOption HostnameOption)
parsePleatHostnameOption = liftA2 handlePleatDisableOption parseHostnameDisabled (HostnameOption <$> parseHostname) 

handlePleatDisableOption :: Bool -> a -> PleatOption a
handlePleatDisableOption False = OptionOn
handlePleatDisableOption True  = const OptionOff

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
