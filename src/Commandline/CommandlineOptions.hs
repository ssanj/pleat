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
      ,  parseDisabledOrFail
      ,  parseGitOption
      ,  parsePleatHostnameOption
      ,  optionStatusToPleatOption
      ,  parseHostname
      ,  parseMaxPathLength
      ,  parseArguments
      ,  parsePrompt
      ,  parsePromptSeparator
      ,  versionHelper
      ) where

import Options.Applicative
import Config

import Paths_pleat (version)
import Development.GitRev (gitHash)
import Data.Semigroup ((<>))

import qualified Data.Version as DV

data OptionStatus = Enabled | Disabled deriving stock (Eq, Show)

newtype PleatVersion = PleatVersion String deriving stock (Eq, Show)
newtype PleatGitHash = PleatGitHash String deriving stock (Eq, Show)

data VersionInfo = VersionInfo { pleatVersion :: PleatVersion, _pleatGitHash :: PleatGitHash } deriving stock (Eq, Show)

parseArguments :: IO Config
parseArguments = execParser pleatInfo

pleatInfo :: ParserInfo Config
pleatInfo =
  info (parseConfig <**> versionHelper <**> helper) (
    fullDesc                                 <>
    header (headerVersionString versionInfo) <>
    progDesc ("Your Bash prompt in Haskell") <>
    footer "---"
  )

headerVersionString :: VersionInfo -> String
headerVersionString (VersionInfo (PleatVersion v) (PleatGitHash h))  = "pleat: " <> v <>  " " <> h

parseConfig :: Parser Config
parseConfig =
  Config <$>
    parsePleatHostnameOption <*>
    parsePathOption          <*>
    parseGitOption           <*>
    parseTimestampOption     <*>
    parsePrompt              <*>
    parsePromptSeparator

parseGitDisabled :: Parser OptionStatus
parseGitDisabled = parseOptionStatus "git"

parseTimestampDisabled :: Parser OptionStatus
parseTimestampDisabled = parseOptionStatus "timestamp"

parseOptionStatus :: String -> Parser OptionStatus
parseOptionStatus optionName =
  flag Enabled Disabled (
    long ("no-" <> optionName) <>
    help ("don't display " <> optionName)
  )

versionHelper :: Parser (a -> a)
versionHelper =
  infoOption (versionString versionInfo)
             (
                short 'v'                 <>
                long "version"            <>
                help "Show pleat version"
             )

versionString :: VersionInfo -> String
versionString (VersionInfo (PleatVersion v) (PleatGitHash h)) = "pleat version " <> v <>  " githash:" <> h

versionInfo :: VersionInfo
versionInfo = VersionInfo (PleatVersion $ DV.showVersion version) (PleatGitHash $(gitHash))

parseGitOption :: Parser (PleatOption GitOption)
parseGitOption = liftA2 optionStatusToPleatOption parseGitDisabled (pure GitOption)

parseTimestampOption :: Parser (PleatOption TimestampOption)
parseTimestampOption = liftA2 optionStatusToPleatOption parseTimestampDisabled (pure TimestampOption)

parsePathOption :: Parser (PleatOption PathOption)
parsePathOption = parsePathDisabled <|> fmap (OptionOn . PathOption)  parseMaxPathLength

optionStatusToPleatOption :: OptionStatus -> a -> PleatOption a
optionStatusToPleatOption Enabled  = OptionOn
optionStatusToPleatOption Disabled = const OptionOff

parsePathDisabled :: Parser (PleatOption PathOption)
parsePathDisabled = parseDisabledOrFail "path"

parseHostnameDisabled :: Parser (PleatOption HostnameOption)
parseHostnameDisabled = parseDisabledOrFail "hostname"

parseDisabledOrFail :: String -> Parser (PleatOption a)
parseDisabledOrFail optionName = flag' OptionOff (long ("no-" <> optionName) <> help ("don't display " <> optionName))

parsePleatHostnameOption :: Parser (PleatOption HostnameOption)
parsePleatHostnameOption = parseHostnameDisabled <|> fmap (OptionOn . HostnameOption) parseHostname

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

parsePromptSeparator :: Parser PromptSeparator
parsePromptSeparator =
  PromptSeparator <$>
    strOption (
      long "prompt-separator"                         <>
      help "override prompt separator"                <>
      showDefault                                     <>
      value (_promptSeparator defaultPromptSeparator) <>
      metavar "SEP"
    )
