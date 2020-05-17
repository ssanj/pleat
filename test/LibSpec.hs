{-# LANGUAGE ScopedTypeVariables #-}

module LibSpec where

import Test.Tasty.HUnit      ((@?=), Assertion)
import Data.Maybe            (catMaybes)
import Data.Functor.Identity (Identity(..))

import qualified Hedgehog              as H
import qualified Hedgehog.Gen          as Gen
import qualified Hedgehog.Range        as Range

import qualified Data.List             as L
import qualified Data.Foldable         as F
-- import qualified Data.List.Split       as L

import qualified Feature.Git       as F
import qualified Feature.Timestamp as F
import qualified Feature.Hostname  as F
import qualified Feature.Path      as F
import qualified Feature.User      as F
import qualified Feature.Prompt    as F
import qualified Feature.Model     as F

import Lib

hprop_mkLoginAtMachine_loginAndMachine :: H.Property
hprop_mkLoginAtMachine_loginAndMachine =
  H.property $ do
    genUsername <- H.forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    genHostname <- H.forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    let user     = F.User genUsername
        hostname = F.Hostname genHostname
        actual   = mkLoginAtMachine (Just user) (Just hostname)
        expected = Just $ LoginAtMachine user hostname
    actual H.=== expected

hprop_mkLoginAtMachine_login :: H.Property
hprop_mkLoginAtMachine_login =
  H.property $ do
    genUsername <- H.forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    let user     = F.User genUsername
        actual   = mkLoginAtMachine (Just user) Nothing
        expected = Just $ Login user
    actual H.=== expected

hprop_mkLoginAtMachine_machine :: H.Property
hprop_mkLoginAtMachine_machine =
  H.property $ do
   genHostname <- H.forAll $ Gen.list (Range.linear 0 100) Gen.alpha
   let hostname = F.Hostname genHostname
       actual   = mkLoginAtMachine Nothing (Just hostname)
       expected = Just $ Machine hostname
   actual H.=== expected

unit_mkLoginAtMachine_neither :: Assertion
unit_mkLoginAtMachine_neither =
  let actual   = mkLoginAtMachine Nothing Nothing
      expected = Nothing :: Maybe Promptable
  in actual @?= expected

hprop_combinePromptablesAllJustShouldBeInOutput :: H.Property
hprop_combinePromptablesAllJustShouldBeInOutput =
  H.property $ do
    maybeStrings <- H.forAll genMaybeStrings
    sep          <- H.forAll genString
    let actual      = combinePromptables id sep maybeStrings
        justStrings = catMaybes maybeStrings
    H.assert $ L.all (`L.isSubsequenceOf` actual) justStrings

hprop_combinePromptablesAllJustMaintainOrderInOutput :: H.Property
hprop_combinePromptablesAllJustMaintainOrderInOutput =
  H.property $ do
    maybeStrings <- H.forAll genMaybeStrings
    let sep      = "" -- without a separator the output should be the concat of [Just input]s
        actual   = combinePromptables id sep maybeStrings
        expected = F.fold $ catMaybes maybeStrings
    actual H.=== expected

-- TODO: move this into its own Spec
-- Then we can test all the different combinations
unit_promptBehaviour :: Assertion
unit_promptBehaviour =
  let localTime      = const $ Identity $ Just $ F.DateTime "[2020-05-18 12:27:53]"
      user           = Identity $ Just $ F.User "jayneway"
      hostname       = const $  Identity $ Just $ F.Hostname "voyager"
      path           = const $  Identity $ Just $ F.Path "/medical/lab/"
      gitBranches    = const $  Identity $ Just $ F.GitBranchModification "[upgrade-hologram --> remote/upgrade-hologram]" ":*"
      promptSuffix   = const $  Just $ F.Prompt "-->"
      behaviour      = F.PromptBehaviour localTime user hostname path gitBranches promptSuffix
      actualPrompt   = promptBehaviour behaviour undefined
      expectedPrompt = "[2020-05-18 12:27:53]:jayneway@voyager:/medical/lab/:[upgrade-hologram --> remote/upgrade-hologram]:*:-->"
  in (runIdentity actualPrompt) @?= expectedPrompt

genString :: H.Gen String
genString = Gen.filter (/= "") $ Gen.string (Range.linear 0 100) Gen.alpha

genMaybeString :: H.Gen (Maybe String)
genMaybeString = Gen.choice [Just <$> genString, Gen.constant Nothing]

genMaybeStrings :: H.Gen [Maybe String]
genMaybeStrings = Gen.list (Range.linear 0 100) genMaybeString

-- list :: MonadGen m => Range Int -> m a -> m [a]
-- forAll :: (Monad m, Show a, HasCallStack) => Gen a -> PropertyT m a
-- property :: HasCallStack => PropertyT IO () -> Property
-- (===) (MonadTest m, Eq a, Show a, HasCallStack) => a -> a -> m ()