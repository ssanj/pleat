module CommandlineOptions (pleatInfo) where

import Options.Applicative
import Config

import Data.Semigroup ((<>))

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
  Config <$> parsePleatHostnameOption <*> parseMaxPathLength

parseHostnameEnabled :: Parser Bool
parseHostnameEnabled = 
  flag False True (
    long "no-hostname" <>
    help "turn off hostname display"
  )

parsePleatHostnameOption :: Parser (PleatOption HostnameOption)
parsePleatHostnameOption = liftA2 handlePleatDisableOption parseHostnameEnabled (HostnameOption <$> parseHostname) 

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
