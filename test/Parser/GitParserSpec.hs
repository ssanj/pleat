{-# LANGUAGE OverloadedStrings #-}

module Parser.GitParserSpec (test_all) where

import Test.Tasty                      (TestTree)
import Test.Tasty                      (testGroup)
import Test.Tasty.HUnit                (assertFailure)
import Test.Tasty.HUnit                (testCase)
import Test.Tasty.HUnit                ((@?=))
import Data.Functor.Identity           (Identity)
import Parser.GitParser                (LocalBranch(..), RemoteBranch(..), localBranch, remoteBranch)
import Text.Parsec                     (parse)

test_all :: TestTree
test_all = 
  testGroup 
    "GitParser" 
    [
       testWithLocalBranch     "* master b93b0b7 More WIP"
    ,  testWithoutLocalBranch  "Merge pull request #1715"
    ,  testWithRemoteBranch    "* master b7fd5fb0 [origin/master] Merge pull request #1715 from jneira/fix-install-hoogle"
    ,  testWithoutRemoteBranch "* master b93b0b7 More WIP"
    ]

testWithLocalBranch :: String -> TestTree
testWithLocalBranch input = 
  let parseResult = parse localBranch "" input
      result      = either (assertFailure . show) (\lbranch -> 
                                                    case lbranch of 
                                                      Just (LocalBranch branchName) -> branchName @?= "master"
                                                      Nothing                       -> assertFailure "Local branch not found"
                                                  ) parseResult
  in testCase "testWithLocalBranch" result

testWithoutLocalBranch :: String -> TestTree
testWithoutLocalBranch input = 
  let parseResult = parse localBranch "" input
      result      = either (assertFailure . show) (\lbranch -> 
                                                    case lbranch of 
                                                      Just (LocalBranch branchName) -> assertFailure $ "expected no remote branch but got: " <> branchName
                                                      Nothing                       -> pure ()
                                                  ) parseResult
  in testCase "testWithoutLocalBranch" result

testWithRemoteBranch :: String -> TestTree
testWithRemoteBranch input = 
  let parseResult = parse remoteBranch "" input
      result      = either (assertFailure . show) (\rBranch -> 
                                                    case rBranch of 
                                                      Just (RemoteBranch remote branchName) -> 
                                                        do 
                                                          remote     @?= "origin" 
                                                          branchName @?= "master"
                                                      Nothing                        -> assertFailure "Remote branch not found"
                                                  ) parseResult
  in testCase "testWithRemoteBranch" result

testWithoutRemoteBranch :: String -> TestTree
testWithoutRemoteBranch input = 
  let parseResult = parse remoteBranch "" input
      result      = either (assertFailure . show) (\rBranch -> 
                                                    case rBranch of 
                                                      Just (RemoteBranch remote branchName) -> assertFailure $ "expected no remote branch but got: " <> remote <> "/" <> branchName
                                                      Nothing                        -> pure ()
                                                  ) parseResult
  in testCase "testWithoutRemoteBranch" result

