module Commandline.CommandlineOptionsSpec where

import Test.Tasty                      (TestTree)
import Test.Tasty                      (testGroup)
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
unit_parseHostnameDisabledWithDisabledFlag = runParser parseHostnameDisabled ["--no-hostname"] True

unit_parseHostnameDisabledWithoutDisabledFlag :: Assertion
unit_parseHostnameDisabledWithoutDisabledFlag = runParser parseHostnameDisabled [] False

unit_parseGitDisabledWithDisabledFlag :: Assertion
unit_parseGitDisabledWithDisabledFlag = runParser parseGitDisabled ["--no-git"] True

-- why does this throw a ParseFailure instead of returning False?
unit_parseGitDisabledWithoutDisabledFlag :: Assertion
unit_parseGitDisabledWithoutDisabledFlag = runParser parseGitDisabled [] False

-- TODO: handlePleatDisableOption could be PBT tests
unit_handlePleatDisableOptionWithDisabled :: Assertion
unit_handlePleatDisableOptionWithDisabled = handlePleatDisableOption True "test" @?= OptionOff

unit_handlePleatDisableOptionWithenabled :: Assertion
unit_handlePleatDisableOptionWithenabled = handlePleatDisableOption False "test" @?= (OptionOn "test")

unit_parseBooleanOptionWithFlag :: Assertion
unit_parseBooleanOptionWithFlag = runParser (parseBooleanOption "whatever") [] False

unit_parseConfig :: Assertion
unit_parseConfig = runParser (parseConfig) [] $ 
  Config { 
            _pleatHostnameOption = OptionOn $ HostnameOption Nothing
         ,  _maxPathLength       = defaultMaxPathLength
         , _pleatGitOption       = OptionOn GitOption
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
      other            -> pure ()
