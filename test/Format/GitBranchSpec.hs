{-# LANGUAGE OverloadedStrings #-}

module Format.GitBranchSpec (test_all) where

import qualified Parser.GitParser as Git

import Test.Tasty                      (TestTree)
import Test.Tasty                      (testGroup)
import Test.Tasty.HUnit                (assertFailure)
import Test.Tasty.HUnit                (testCase)
import Test.Tasty.HUnit                ((@?=))
import Data.Functor.Identity           (Identity)
import Text.Parsec                     (parse)

import Format.GitBranch

test_all :: TestTree
test_all = 
  testGroup 
    "GitParser" 
    [
       processGitBranchWithEmptyInput
    ,  processGitBranchWithNoInput
    ,  processGitBranchWithoutActiveBranch
    ,  processGitBranchWithRemoteBranch
    ,  processGitBranchWithLocalBranch
    ,  processGitBranchWithUnparsableInput
    ]

processGitBranchWithEmptyInput :: TestTree
processGitBranchWithEmptyInput = 
  withTestCase "processGitBranchWithEmptyInput" $ 
    processGitBranch (Just []) @?= (Just InitialCommit)

processGitBranchWithNoInput :: TestTree
processGitBranchWithNoInput = 
  withTestCase "processGitBranchWithNoInput" $ 
    processGitBranch Nothing @?= Nothing

processGitBranchWithoutActiveBranch :: TestTree
processGitBranchWithoutActiveBranch = 
  withTestCase "processGitBranchWithoutActiveBranch" $ 
    processGitBranch (Just [
                              "2                          3d5ae73 Release 0.0.9-b01"
                            , "add-macro-support          4cf335e [origin/add-macro-support] Choose Exception syntax"
                            , "alternate-exception-syntax d7a99e8 [origin/alternate-exception-syntax] Alternate Exception Syntax"
                            , "composite-assertions       6757633 [origin/composite-assertions] Add travis"
                           ]) @?= Nothing

processGitBranchWithRemoteBranch :: TestTree
processGitBranchWithRemoteBranch = 
  withTestCase "processGitBranchWithRemoteBranch" $ 
    processGitBranch (Just [
                              "2                          3d5ae73 Release 0.0.9-b01"
                            , "add-macro-support          4cf335e [origin/add-macro-support] Choose Exception syntax"
                            , "alternate-exception-syntax d7a99e8 [origin/alternate-exception-syntax] Alternate Exception Syntax"
                            , "* do-something-else                     3d5ae73 [moon/do-something-else] Release 0.0.9-b01"
                            , "composite-assertions       6757633 [origin/composite-assertions] Add travis"
                           ]) @?= Just (Remote (Git.RemoteBranch "moon" "do-something-else"))

processGitBranchWithLocalBranch :: TestTree
processGitBranchWithLocalBranch = 
  withTestCase "processGitBranchWithLocalBranch" $ 
    processGitBranch (Just ["* slave 99d80d9f Merge pull request #1827 from scala-steward/update/sbt-1.3.9"]) 
      @?= Just (Local (Git.LocalBranch "slave"))

processGitBranchWithUnparsableInput :: TestTree
processGitBranchWithUnparsableInput = 
  withTestCase "processGitBranchWithUnparsableInput" $ 
    processGitBranch (Just [""]) @?= Nothing

withTestCase :: String -> IO () -> TestTree
withTestCase name = testCase name
