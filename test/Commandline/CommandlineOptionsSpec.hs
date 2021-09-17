module Commandline.CommandlineOptionsSpec where

import Test.Tasty.HUnit                (assertFailure, assertBool, (@?=), Assertion)
import Data.Semigroup                  ((<>))

import qualified Data.Text as T

import Commandline.CommandlineOptions
import Options.Applicative
import Options.Applicative.Types
import Options.Applicative.Help.Chunk (unChunk)
import Config

newtype MetaVar = MetaVar String
newtype HelpText = HelpText String
newtype ModName = ModName String
newtype DefaultVal = DefaultVal { _defaultVal :: String }

unit_parsesHostname :: Assertion
unit_parsesHostname =
  runParser parseHostname ["--hostname", "DreamMachine"] $ Just (Hostname . T.pack $ "DreamMachine")

unit_parsesMaxPathLength :: Assertion
unit_parsesMaxPathLength =
  runParser parseMaxPathLength ["--max-path-length", "10"] $ MaxPathLength 10

unit_parsesMaxPathLengthWithDefault :: Assertion
unit_parsesMaxPathLengthWithDefault = runParser parseMaxPathLength [] defaultMaxPathLength

unit_parseHostnameDisabledWithDisabledFlag :: Assertion
unit_parseHostnameDisabledWithDisabledFlag = runParser parseHostnameDisabled ["--no-hostname"] OptionOff

unit_parseHostnameDisabledWithoutDisabledFlag :: Assertion
unit_parseHostnameDisabledWithoutDisabledFlag = runParserWithFailure parseHostnameDisabled [] "Missing: --no-hostname"

unit_parseGitDisabledWithDisabledFlag :: Assertion
unit_parseGitDisabledWithDisabledFlag = runParser parseGitDisabled ["--no-git"] Disabled

unit_parseGitDisabledWithoutDisabledFlag :: Assertion
unit_parseGitDisabledWithoutDisabledFlag = runParser parseGitDisabled [] Enabled

unit_parseTimestampDisabledWithDisabledFlag :: Assertion
unit_parseTimestampDisabledWithDisabledFlag = runParser parseTimestampDisabled ["--no-timestamp"] Disabled

unit_parseTimestampDisabledWithoutDisabledFlag :: Assertion
unit_parseTimestampDisabledWithoutDisabledFlag = runParser parseTimestampDisabled [] Enabled

unit_parsePathDisabledWithDisabledFlag :: Assertion
unit_parsePathDisabledWithDisabledFlag = runParser parsePathDisabled ["--no-path"] OptionOff

unit_parsePathDisabledWithoutDisabledFlag :: Assertion
unit_parsePathDisabledWithoutDisabledFlag = runParserWithFailure parsePathDisabled [] "Missing: --no-path"

-- TODO: handlePleatDisableOption could be PBT tests
unit_handlePleatDisableOptionWithDisabled :: Assertion
unit_handlePleatDisableOptionWithDisabled = optionStatusToPleatOption Disabled "test" @?= OptionOff


unit_handleParseDisabledOrFailWithoutFlag :: Assertion
unit_handleParseDisabledOrFailWithoutFlag = runParserWithFailure (parseDisabledOrFail "test-flag" :: Parser (PleatOption String)) [] "Missing: --no-test-flag"

unit_handleParseDisabledOrFailWithFlag :: Assertion
unit_handleParseDisabledOrFailWithFlag = runParser (parseDisabledOrFail "test-flag" :: Parser (PleatOption String)) ["--no-test-flag"] OptionOff

unit_optionStatusToPleatOptionWithEnabled :: Assertion
unit_optionStatusToPleatOptionWithEnabled = optionStatusToPleatOption Enabled "test" @?= (OptionOn "test")

unit_parseOptionStatusWithoutDisabledFlag :: Assertion
unit_parseOptionStatusWithoutDisabledFlag = runParser (parseOptionStatus "whatever") [] Enabled

unit_parseOptionStatusWithDisabledFlag :: Assertion
unit_parseOptionStatusWithDisabledFlag = runParser (parseOptionStatus "whatever") ["--no-whatever"] Disabled

unit_parsePrompt :: Assertion
unit_parsePrompt = runParser parsePrompt ["--prompt", "|> "] (Prompt . T.pack $ "|> ")

unit_parsePromptSeparator :: Assertion
unit_parsePromptSeparator = runParser parsePromptSeparator ["--prompt-separator", "#"] (PromptSeparator . T.pack $ "#")

unit_parseConfig :: Assertion
unit_parseConfig = runParser parseConfig [] $
  PleatConfigCommand $
    Config {
       _pleatHostnameOverrideOption = OptionOn $ HostnameOption Nothing
    ,  _pleatPathOption             = OptionOn $ PathOption $ defaultMaxPathLength
    , _pleatGitOption               = OptionOn GitOption
    , _pleatTimestampOption         = OptionOn TimestampOption
    , _pleatPrompt                  = defaultPrompt
    , _pleatPromptSeparator         = defaultPromptSeparator
    }

unit_versionMod :: Assertion
unit_versionMod =
  assertParserMod
    versionHelper
    [OptLong "version", OptShort 'v']
    (HelpText "Show pleat version")
    (MetaVar "")
    Nothing
    (ModName "version")

unit_promptSeparatorMod :: Assertion
unit_promptSeparatorMod =
    assertParserMod
    parsePromptSeparator
    [OptLong "prompt-separator"]
    (HelpText "override prompt separator")
    (MetaVar "SEP")
    (Just $ DefaultVal "\":\"")
    (ModName "prompt separator")

-- TODO: Add visibility
assertParserMod :: Parser a -> [OptName] -> HelpText -> MetaVar -> (Maybe DefaultVal) -> ModName -> Assertion
assertParserMod parserOfA expectedOptionNames (HelpText expectedHelpText) (MetaVar expectedMetaVar) maybeDefaultVal (ModName modName) =
  let verifyParserMod :: [OptName] -> OptProperties -> Assertion
      verifyParserMod options optProperties =
        do
          let optionNameError = "did not contain all option names, actual: "
                                <> (show options)
                                <> ", expected: "
                                <> (show expectedOptionNames)
          assertBool optionNameError $ all (`elem` options) expectedOptionNames
          maybe (assertFailure "invalid help text") (\y -> (show y) @?= expectedHelpText) (unChunk . propHelp $ optProperties)
          (propMetaVar optProperties) @?= expectedMetaVar
          case (propShowDefault optProperties, maybeDefaultVal) of
            (Just actualDefaultProp, Just expectedDefaultVal) -> actualDefaultProp @?= _defaultVal expectedDefaultVal
            (Nothing,                Nothing)                 -> pure ()
            (actualDefaultProp,      expectedDefaultVal)      -> assertFailure $
                                                                  "invalid default value, actual: "
                                                                  <> (show actualDefaultProp)
                                                                  <> ", expected: "
                                                                  <> (show $ fmap _defaultVal expectedDefaultVal)
  in case parserOfA of
    (AltP (OptP (Option (OptReader options _ _) optProperties)) _) -> verifyParserMod options optProperties
    (AltP (OptP (Option (FlagReader options _)  optProperties)) _) -> verifyParserMod options optProperties
    _ -> assertFailure $ "xinvalid Mods for " <> modName


  -- = NilP (Maybe a)
  -- | OptP (Option a)
  -- | forall x . MultP (Parser (x -> a)) (Parser x)
  -- | AltP (Parser a) (Parser a)
  -- | forall x . BindP (Parser x) (x -> Parser a)

-- TODO: how can I test the various Info options for the Parser like long, help and metavar
runParser :: (Show a, Eq a) => Parser a -> [String] -> a -> Assertion
runParser parserA args expected =
  let result = execParserPure defaultPrefs (info parserA $ header "some-header") args
  in case result of
      (Success actual) -> actual @?= expected
      other            -> assertFailure $ "got error: " <> (show other)

runParserWithFailure :: Show a => Parser a -> [String] -> String -> Assertion
runParserWithFailure parserA args expectedError =
  let result = execParserPure defaultPrefs (info parserA $ header "some-header") args
  in case result of
      (Success actual) -> assertFailure $ "got success but expected an error: " <> (show actual)
      (Failure (ParserFailure errorF))      -> (maybe "-" show . unChunk . helpError . (\(h, _, _)  -> h) . errorF $ "whatever-whatever") @?= expectedError
      (CompletionInvoked _)   -> assertFailure $ "got CompletionInvoked but expected an error"
