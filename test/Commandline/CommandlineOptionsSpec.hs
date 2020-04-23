module Commandline.CommandlineOptionsSpec where

import Test.Tasty.HUnit                (assertFailure, (@?=), Assertion)
import Data.Semigroup                  ((<>))

import Commandline.CommandlineOptions
import Options.Applicative
import Config

unit_parsesHostname :: Assertion
unit_parsesHostname = 
  runParser parseHostname ["--hostname", "DreamMachine"] $ Just (Hostname "DreamMachine")

unit_parsesMaxPathLength :: Assertion
unit_parsesMaxPathLength = 
  runParser parseMaxPathLength ["--max-path-length", "10"] $ MaxPathLength 10

unit_parsesMaxPathLengthWithDefault :: Assertion
unit_parsesMaxPathLengthWithDefault = runParser parseMaxPathLength [] defaultMaxPathLength

unit_parseHostnameDisabledWithDisabledFlag :: Assertion
unit_parseHostnameDisabledWithDisabledFlag = runParser parseHostnameDisabled ["--no-hostname"] Disabled

unit_parseHostnameDisabledWithoutDisabledFlag :: Assertion
unit_parseHostnameDisabledWithoutDisabledFlag = runParser parseHostnameDisabled [] Enabled

unit_parseGitDisabledWithDisabledFlag :: Assertion
unit_parseGitDisabledWithDisabledFlag = runParser parseGitDisabled ["--no-git"] Disabled

unit_parseGitDisabledWithoutDisabledFlag :: Assertion
unit_parseGitDisabledWithoutDisabledFlag = runParser parseGitDisabled [] Enabled

unit_parseTimestampDisabledWithDisabledFlag :: Assertion
unit_parseTimestampDisabledWithDisabledFlag = runParser parseTimestampDisabled ["--no-timestamp"] Disabled

-- TODO: handlePleatDisableOption could be PBT tests  
unit_handlePleatDisableOptionWithDisabled :: Assertion
unit_handlePleatDisableOptionWithDisabled = optionStatusToPleatOption Disabled "test" @?= OptionOff

unit_optionStatusToPleatOptionWithEnabled :: Assertion
unit_optionStatusToPleatOptionWithEnabled = optionStatusToPleatOption Enabled "test" @?= (OptionOn "test")

unit_parseOptionStatusWithoutDisabledFlag :: Assertion
unit_parseOptionStatusWithoutDisabledFlag = runParser (parseOptionStatus "whatever") [] Enabled

unit_parseOptionStatusWithDisabledFlag :: Assertion
unit_parseOptionStatusWithDisabledFlag = runParser (parseOptionStatus "whatever") ["--no-whatever"] Disabled

unit_parsePrompt :: Assertion
unit_parsePrompt = runParser parsePrompt ["--prompt", "|> "] (Prompt "|> ")

unit_parseConfig :: Assertion
unit_parseConfig = runParser (parseConfig) [] $ 
  Config { 
            _pleatHostnameOption = OptionOn $ HostnameOption Nothing
         ,  _maxPathLength       = defaultMaxPathLength
         , _pleatGitOption       = OptionOn GitOption
         , _pleatTimestampOption = OptionOn TimestampOption
         , _pleatPrompt          = defaultPrompt
         }

-- TODO: how can I test the various Info options for the Parser like long, help and metavar
runParser :: (Show a, Eq a) => Parser a -> [String] -> a -> Assertion
runParser parserA args expected =
  let result = execParserPure defaultPrefs (info parserA $ header "some-header") args
  in case result of
      (Success actual) -> actual @?= expected
      other            -> assertFailure $ "got error: " <> (show other)

xrunParserWithFailure :: Show a => Parser a -> [String] -> Assertion
xrunParserWithFailure parserA args =
  let result = execParserPure defaultPrefs (info parserA $ header "some-header") args
  in case result of
      (Success actual) -> assertFailure $ "got success but expected an error: " <> (show actual)
      _            -> pure ()
