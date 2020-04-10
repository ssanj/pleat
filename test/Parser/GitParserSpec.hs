{-# LANGUAGE OverloadedStrings #-}

module Parser.GitParserSpec where

import Test.Tasty                      (TestTree, testGroup)
import Test.Tasty.HUnit                (Assertion, assertFailure, testCase, (@?=))
import Data.Functor.Identity           (Identity)
import Parser.GitParser                (LocalBranch(..), RemoteBranch(..), LocalAndRemoteBranch(..), localBranch, remoteBranch, localAndRemoteBranch)
import Text.Parsec                     (parse)

unit_parseWithLocalBranch :: Assertion
unit_parseWithLocalBranch =
  let parseResult = parse localBranch "" "* master b93b0b7 More WIP" in
  either (assertFailure . show) (\lbranch -> 
                                    case lbranch of 
                                      Just (LocalBranch branchName) -> branchName @?= "master"
                                      Nothing                       -> assertFailure "Local branch not found"
                                ) parseResult

unit_parseWithoutActiveBranch :: Assertion
unit_parseWithoutActiveBranch = 
  let parseResult = parse localBranch "" "Merge pull request #1715" in
  either (assertFailure . show) (\lbranch -> 
                                    case lbranch of 
                                      Just (LocalBranch branchName) -> assertFailure $ "expected no remote branch but got: " <> branchName
                                      Nothing                       -> pure ()
                                ) parseResult

unit_parseWithRemoteBranch :: Assertion
unit_parseWithRemoteBranch = 
  let parseResult = parse remoteBranch "" "* master b7fd5fb0 [origin/master] Merge pull request #1715 from jneira/fix-install-hoogle" in
  either (assertFailure . show) (\rBranch -> 
                                    case rBranch of 
                                      Just (RemoteBranch remote branchName) -> 
                                        do 
                                          remote     @?= "origin" 
                                          branchName @?= "master"
                                      Nothing                        -> assertFailure "Remote branch not found"
                                ) parseResult

unit_parseWithoutRemoteBranch :: Assertion
unit_parseWithoutRemoteBranch = 
  let parseResult = parse remoteBranch "" "* master b93b0b7 More WIP" in
  either (assertFailure . show) (\rBranch -> 
                                    case rBranch of 
                                      Just (RemoteBranch remote branchName) -> assertFailure $ "expected no remote branch but got: " <> remote <> "/" <> branchName
                                      Nothing                               -> pure ()
                                ) parseResult

unit_parseWithLocalAndRemoteBranch :: Assertion
unit_parseWithLocalAndRemoteBranch = 
  let parseResult = parse localAndRemoteBranch "" "* slave b7fd5fb0 [origin/master] Merge pull request #1715 from jneira/fix-install-hoogle" in
  either (assertFailure . show) (\rBranch -> 
                                    case rBranch of 
                                      Just (LocalAndRemoteBranch (LocalBranch localBranchName) (Just (RemoteBranch remote remoteBranchName))) -> 
                                        do 
                                          localBranchName  @?= "slave"
                                          remote           @?= "origin" 
                                          remoteBranchName @?= "master"
                                      Nothing                        -> assertFailure "Could not parse both Local and Remote branches as LocalAndRemoteBranch"
                                ) parseResult

unit_parseWithLocalAndRemoteBranchWithoutRemoteBranch :: Assertion
unit_parseWithLocalAndRemoteBranchWithoutRemoteBranch = 
  let parseResult = parse localAndRemoteBranch "" "* master b7fd5fb0 Merge pull request #1715 from jneira/fix-install-hoogle" in
  either (assertFailure . show) (\rBranch -> 
                                    case rBranch of 
                                      Just (LocalAndRemoteBranch (LocalBranch localBranchName) rb) -> 
                                        do 
                                          localBranchName  @?= "master"
                                          rb               @?= Nothing
                                      Nothing                        -> assertFailure "Could not parse as LocalAndRemoteBranch with only local branch"
                                ) parseResult

unit_parseWithLocalAndRemoteBranchWithoutLocalBranch :: Assertion
unit_parseWithLocalAndRemoteBranchWithoutLocalBranch = 
  let parseResult = parse localAndRemoteBranch "" "master b7fd5fb0 Merge pull request #1715 from jneira/fix-install-hoogle" in
  either (assertFailure . show) ((@?= Nothing)) parseResult
