module CommandlineOptions (pleatInfo) where

import Options.Applicative
import Config

import Data.Semigroup ((<>))

pleatInfo :: ParserInfo Config
pleatInfo = 
  info (parseConfig <**> helper) (
    fullDesc <>
    progDesc "writes out a bash prompt with useful information" <>
    header "pleat - bash prompt"
  )

parseConfig :: Parser Config
parseConfig = 
  Config <$> parseHostname <*> parseMaxPathLength

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
