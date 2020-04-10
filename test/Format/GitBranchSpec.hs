{-# LANGUAGE OverloadedStrings #-}

module Format.GitBranchSpec where

import qualified Parser.GitParser as Git

import Test.Tasty                      (TestTree)
import Test.Tasty                      (testGroup)
import Test.Tasty.HUnit                (assertFailure, testCase, (@?=), Assertion)
import Data.Functor.Identity           (Identity)
import Text.Parsec                     (parse)

import Format.GitBranch

unit_processGitBranchWithEmptyInput :: Assertion
unit_processGitBranchWithEmptyInput = processGitBranch (Just []) @?= (Just InitialCommit)

unit_processGitBranchWithNoInput :: Assertion
unit_processGitBranchWithNoInput = processGitBranch Nothing @?= Nothing

unit_processGitBranchWithRemoteBranch :: Assertion
unit_processGitBranchWithRemoteBranch = 
  processGitBranch (Just [
                            "2                          3d5ae73 Release 0.0.9-b01"
                          , "add-macro-support          4cf335e [origin/add-macro-support] Choose Exception syntax"
                          , "alternate-exception-syntax d7a99e8 [origin/alternate-exception-syntax] Alternate Exception Syntax"
                          , "* do-something-else                     3d5ae73 [moon/do-something-else] Release 0.0.9-b01"
                          , "composite-assertions       6757633 [origin/composite-assertions] Add travis"
                         ]) @?= Just (Remote (Git.RemoteBranch "moon" "do-something-else"))

unit_processGitBranchWithLocalBranch :: Assertion
unit_processGitBranchWithLocalBranch = 
  processGitBranch (Just ["* slave 99d80d9f Merge pull request #1827 from scala-steward/update/sbt-1.3.9"]) 
    @?= Just (Local (Git.LocalBranch "slave"))

unit_processGitBranchWithUnparsableInput :: Assertion
unit_processGitBranchWithUnparsableInput = processGitBranch (Just [""]) @?= Nothing
