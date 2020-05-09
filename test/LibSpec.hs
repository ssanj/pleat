module LibSpec where

import Test.Tasty.HUnit ((@?=), Assertion)

import qualified Hedgehog              as H
import qualified Hedgehog.Gen          as H
import qualified Hedgehog.Range        as H


-- import qualified Feature.Git       as F
-- import qualified Feature.Timestamp as F
import qualified Feature.Hostname  as F
-- import qualified Feature.Path      as F
import qualified Feature.User      as F
-- import qualified Feature.Prompt    as F

import Lib

hprop_mkLoginAtMachine_loginAndMachine :: H.Property
hprop_mkLoginAtMachine_loginAndMachine =
  H.property $ do
    genUsername <- H.forAll $ H.list (H.linear 0 100) H.alpha
    genHostname <- H.forAll $ H.list (H.linear 0 100) H.alpha
    let user     = F.User genUsername
        hostname = F.Hostname genHostname
        actual   = mkLoginAtMachine (Just user) (Just hostname) 
        expected = Just $ LoginAtMachine user hostname
    actual H.=== expected

hprop_mkLoginAtMachine_login :: H.Property
hprop_mkLoginAtMachine_login =
  H.property $ do
    genUsername <- H.forAll $ H.list (H.linear 0 100) H.alpha
    let user     = F.User genUsername
        actual   = mkLoginAtMachine (Just user) Nothing
        expected = Just $ Login user
    actual H.=== expected

hprop_mkLoginAtMachine_machine :: H.Property
hprop_mkLoginAtMachine_machine =
  H.property $ do
   genHostname <- H.forAll $ H.list (H.linear 0 100) H.alpha
   let hostname = F.Hostname genHostname
       actual   = mkLoginAtMachine Nothing (Just hostname) 
       expected = Just $ Machine hostname
   actual H.=== expected

unit_mkLoginAtMachine_neither :: Assertion
unit_mkLoginAtMachine_neither =
  let actual   = mkLoginAtMachine Nothing Nothing
      expected = Nothing :: Maybe Promptable
  in actual @?= expected

