{-# LANGUAGE ScopedTypeVariables #-}

module Program.ProgramSpec where

import Test.Tasty.HUnit ((@?=), Assertion)
-- import Data.Maybe       (catMaybes)

import qualified Hedgehog              as H
import qualified Hedgehog.Gen          as Gen
import qualified Hedgehog.Range        as Range

import qualified Data.Text             as T
import qualified Feature.Feature       as F

import Program.Program

hprop_mkLoginAtMachine_loginAndMachine :: H.Property
hprop_mkLoginAtMachine_loginAndMachine =
  H.property $ do
    genUsername <- H.forAll $ Gen.text (Range.linear 0 100) Gen.alpha
    genHostname <- H.forAll $ Gen.text (Range.linear 0 100) Gen.alpha
    let user     = F.User genUsername
        hostname = F.Hostname genHostname
        actual   = mkLoginAtMachine (Just user) (Just hostname)
        expected = Just $ LoginAtMachine user hostname
    actual H.=== expected

hprop_mkLoginAtMachine_login :: H.Property
hprop_mkLoginAtMachine_login =
  H.property $ do
    genUsername <- H.forAll $ Gen.text (Range.linear 0 100) Gen.alpha
    let user     = F.User genUsername
        actual   = mkLoginAtMachine (Just user) Nothing
        expected = Just $ Login user
    actual H.=== expected

hprop_mkLoginAtMachine_machine :: H.Property
hprop_mkLoginAtMachine_machine =
  H.property $ do
   genHostname <- H.forAll $ Gen.text (Range.linear 0 100) Gen.alpha
   let hostname = F.Hostname genHostname
       actual   = mkLoginAtMachine Nothing (Just hostname)
       expected = Just $ Machine hostname
   actual H.=== expected

unit_mkLoginAtMachine_neither :: Assertion
unit_mkLoginAtMachine_neither =
  let actual   = mkLoginAtMachine Nothing Nothing
      expected = Nothing :: Maybe Promptable
  in actual @?= expected

-- hprop_combinePromptablesAllJustShouldBeInOutput :: H.Property
-- hprop_combinePromptablesAllJustShouldBeInOutput =
--   H.property $ do
--     sep          <- H.forAll $ genStringIgnoring [T.pack ""]
--     maybeStrings <- H.forAll $ genMaybeStrings [sep]
--     let combined    = combinePromptables id sep maybeStrings
--         justStrings = catMaybes maybeStrings
--         actual      = T.splitOn sep combined
--     H.assert $ actual == justStrings

-- hprop_combinePromptablesAllJustMaintainOrderInOutput :: H.Property
-- hprop_combinePromptablesAllJustMaintainOrderInOutput =
--   H.property $ do
--     maybeStrings <- H.forAll genMaybeStrings
--     let sep      = T.pack "" -- without a separator the output should be the concat of [Just input]s
--         actual   = combinePromptables id sep maybeStrings
--         expected = F.fold $ catMaybes maybeStrings
--     actual H.=== expected

genStringIgnoring :: [T.Text] -> H.Gen T.Text
genStringIgnoring ignored = Gen.filter (\genChar -> not $ any (== genChar) ignored) $ Gen.text (Range.linear 1 100) Gen.alpha

genMaybeString :: [T.Text] -> H.Gen (Maybe T.Text)
genMaybeString ignored = Gen.choice [Just <$> genStringIgnoring ignored, Gen.constant Nothing]

genMaybeStrings :: [T.Text] -> H.Gen [Maybe T.Text]
genMaybeStrings ignored = Gen.list (Range.linear 1 100) $ genMaybeString ignored

-- list :: MonadGen m => Range Int -> m a -> m [a]
-- forAll :: (Monad m, Show a, HasCallStack) => Gen a -> PropertyT m a
-- property :: HasCallStack => PropertyT IO () -> Property
-- (===) (MonadTest m, Eq a, Show a, HasCallStack) => a -> a -> m ()