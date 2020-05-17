
module LibPromptBehaviourSpec where

import Test.Tasty.HUnit      ((@?=), Assertion)
import Data.Functor.Identity (Identity(..))

-- import qualified Hedgehog              as H
-- import qualified Hedgehog.Gen          as Gen
-- import qualified Hedgehog.Range        as Range

import qualified Feature.Git       as F
import qualified Feature.Timestamp as F
import qualified Feature.Hostname  as F
import qualified Feature.Path      as F
import qualified Feature.User      as F
import qualified Feature.Prompt    as F
import qualified Feature.Model     as F

import Lib

-- TODO: test all the different combinations
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

-- list :: MonadGen m => Range Int -> m a -> m [a]
-- forAll :: (Monad m, Show a, HasCallStack) => Gen a -> PropertyT m a
-- property :: HasCallStack => PropertyT IO () -> Property
-- (===) (MonadTest m, Eq a, Show a, HasCallStack) => a -> a -> m ()