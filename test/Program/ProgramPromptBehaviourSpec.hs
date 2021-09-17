module Program.ProgramPromptBehaviourSpec where

import Test.Tasty.HUnit      ((@?=), Assertion)
import Data.Functor.Identity (Identity(..))

import qualified Data.Text as T

-- import qualified Hedgehog              as H
-- import qualified Hedgehog.Gen          as Gen
-- import qualified Hedgehog.Range        as Range

import qualified Feature.Feature as F
import Program.Program

unit_promptBehaviourAllFeatures :: Assertion
unit_promptBehaviourAllFeatures =
  let behaviour      = allFeatures
      actualPrompt   = promptBehaviour behaviour undefined
      expectedPrompt = "[2020-05-18 12:27:53]:jayneway@voyager:/medical/lab/:[upgrade-hologram --> remote/upgrade-hologram]:*:-->"
  in (runIdentity actualPrompt) @?= expectedPrompt

unit_promptBehaviourAllFeaturesNoTime :: Assertion
unit_promptBehaviourAllFeaturesNoTime =
  let behaviour      = allFeatures { F._processTimestamp = const $ Identity Nothing }
      actualPrompt   = promptBehaviour behaviour undefined
      expectedPrompt = "jayneway@voyager:/medical/lab/:[upgrade-hologram --> remote/upgrade-hologram]:*:-->"
  in (runIdentity actualPrompt) @?= expectedPrompt

unit_promptBehaviourAllFeaturesNoUser :: Assertion
unit_promptBehaviourAllFeaturesNoUser =
  let behaviour      = allFeatures { F._processUser = Identity Nothing }
      actualPrompt   = promptBehaviour behaviour undefined
      expectedPrompt = "[2020-05-18 12:27:53]:voyager:/medical/lab/:[upgrade-hologram --> remote/upgrade-hologram]:*:-->"
  in (runIdentity actualPrompt) @?= expectedPrompt

unit_promptBehaviourAllFeaturesNoHostname :: Assertion
unit_promptBehaviourAllFeaturesNoHostname =
  let behaviour      = allFeatures { F._processHostname = const $ Identity Nothing }
      actualPrompt   = promptBehaviour behaviour undefined
      expectedPrompt = "[2020-05-18 12:27:53]:jayneway:/medical/lab/:[upgrade-hologram --> remote/upgrade-hologram]:*:-->"
  in (runIdentity actualPrompt) @?= expectedPrompt

unit_promptBehaviourAllFeaturesNoPath :: Assertion
unit_promptBehaviourAllFeaturesNoPath =
  let behaviour      = allFeatures { F._processPath = const $ Identity Nothing }
      actualPrompt   = promptBehaviour behaviour undefined
      expectedPrompt = "[2020-05-18 12:27:53]:jayneway@voyager:[upgrade-hologram --> remote/upgrade-hologram]:*:-->"
  in (runIdentity actualPrompt) @?= expectedPrompt

unit_promptBehaviourAllFeaturesNoGit :: Assertion
unit_promptBehaviourAllFeaturesNoGit =
  let behaviour      = allFeatures { F._processGitRepo = const $ Identity Nothing }
      actualPrompt   = promptBehaviour behaviour undefined
      expectedPrompt = "[2020-05-18 12:27:53]:jayneway@voyager:/medical/lab/:-->"
  in (runIdentity actualPrompt) @?= expectedPrompt

unit_promptBehaviourAllFeaturesNoPrompt :: Assertion
unit_promptBehaviourAllFeaturesNoPrompt =
  let behaviour      = allFeatures { F._processPromptSuffix = const Nothing }
      actualPrompt   = promptBehaviour behaviour undefined
      expectedPrompt = "[2020-05-18 12:27:53]:jayneway@voyager:/medical/lab/:[upgrade-hologram --> remote/upgrade-hologram]:*"
  in (runIdentity actualPrompt) @?= expectedPrompt

allFeatures :: F.PromptBehaviour Identity
allFeatures =
  let localTime       = const $ Identity $ Just $ F.DateTime . T.pack $ "[2020-05-18 12:27:53]"
      user            = Identity $ Just $ F.User . T.pack $ "jayneway"
      hostname        = const $  Identity $ Just $ F.Hostname . T.pack $ "voyager"
      path            = const $  Identity $ Just $ F.Path . T.pack $ "/medical/lab/"
      gitBranches     = const $  Identity $ Just $ F.GitBranchModification (T.pack "[upgrade-hologram --> remote/upgrade-hologram]") (T.pack ":*")
      promptSuffix    = const $  Just $ F.Prompt . T.pack $ "-->"
      promptSeparator = const $ F.PromptSeparator . T.pack $ ":"
  in F.PromptBehaviour localTime user hostname path gitBranches promptSuffix promptSeparator
