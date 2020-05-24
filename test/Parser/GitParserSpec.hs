{-# LANGUAGE OverloadedStrings #-}

module Parser.GitParserSpec where

import Test.Tasty.HUnit                (Assertion, assertFailure, (@?=))
import Text.Parsec                     (parse, option)

import Parser.GitParser

unit_parseWithLocalBranch :: Assertion
unit_parseWithLocalBranch =
  let parseResult = parse localBranch "" "* master b93b0b7 More WIP" in
  either (assertFailure . show) (\lbranch ->
                                    case lbranch of
                                      Just (LocalBranch branchName (GitHash hash)) ->
                                        do
                                          branchName @?= "master"
                                          hash       @?= "b93b0b7"
                                      Nothing                                      -> assertFailure "Local branch not found"
                                ) parseResult

unit_parseWithoutActiveBranch :: Assertion
unit_parseWithoutActiveBranch =
  let parseResult = parse localBranch "" "Merge pull request #1715" in
  either (assertFailure . show) (\lbranch ->
                                    case lbranch of
                                      Just lb@(LocalBranch _ _) -> assertFailure $ "expected no remote branch but got: " <> (show lb)
                                      Nothing                   -> pure ()
                                ) parseResult

unit_parseWithRemoteBranch :: Assertion
unit_parseWithRemoteBranch =
  let parseResult = parse remoteBranch "" "* master b7fd5fb0 [origin/master] Merge pull request #1715 from jneira/fix-install-hoogle" in
  either (assertFailure . show) (\rBranch ->
                                    case rBranch of
                                      Just (RemoteBranch remote branchName ahead) ->
                                        do
                                          remote     @?= "origin"
                                          branchName @?= "master"
                                          ahead      @?= Nothing
                                      Nothing                                 -> assertFailure "Remote branch not found"
                                ) parseResult

unit_parseWithRemoteBranchAndUnpushedUpdates :: Assertion
unit_parseWithRemoteBranchAndUnpushedUpdates =
  let parseResult = parse remoteBranch "" "* master b7fd5fb0 [origin/master: ahead 1] Merge pull request #1715 from jneira/fix-install-hoogle" in
  either (assertFailure . show) (\rBranch ->
                                    case rBranch of
                                      Just (RemoteBranch remote branchName ahead) ->
                                        do
                                          remote     @?= "origin"
                                          branchName @?= "master"
                                          ahead      @?= Just (CommitsAhead 1)
                                      Nothing                        -> assertFailure "Remote branch not found"
                                ) parseResult

unit_parseWithoutRemoteBranch :: Assertion
unit_parseWithoutRemoteBranch =
  let parseResult = parse remoteBranch "" "* master b93b0b7 More WIP" in
  either (assertFailure . show) (\rBranch ->
                                    case rBranch of
                                      Just (RemoteBranch remote branchName _) -> assertFailure $ "expected no remote branch but got: " <> remote <> "/" <> branchName
                                      Nothing                               -> pure ()
                                ) parseResult

unit_parseWithLocalAndRemoteBranch :: Assertion
unit_parseWithLocalAndRemoteBranch =
  let parseResult = parse localAndRemoteBranch "" "* slave b7fd5fb0 [origin/master] Merge pull request #1715 from jneira/fix-install-hoogle" in
  either (assertFailure . show) (\rBranch ->
                                    case rBranch of
                                      Just (LocalAndRemoteBranch (LocalBranch localBranchName (GitHash hash)) (Just (RemoteBranch remote remoteBranchName ahead))) ->
                                        do
                                          localBranchName  @?= "slave"
                                          remote           @?= "origin"
                                          remoteBranchName @?= "master"
                                          hash             @?= "b7fd5fb0"
                                          ahead            @?= Nothing
                                      Just other -> assertFailure $ "Could not parse both Local and Remote branches as LocalAndRemoteBranch: " <> (show other)
                                      Nothing    -> assertFailure "Could not parse both Local and Remote branches as LocalAndRemoteBranch"
                                ) parseResult

unit_parseWithLocalAndRemoteBranchWithoutRemoteBranch :: Assertion
unit_parseWithLocalAndRemoteBranchWithoutRemoteBranch =
  let parseResult = parse localAndRemoteBranch "" "* master b7fd5fb0 Merge pull request #1715 from jneira/fix-install-hoogle" in
  either (assertFailure . show) (\rBranch ->
                                    case rBranch of
                                      Just (LocalAndRemoteBranch (LocalBranch localBranchName (GitHash hash)) rb) ->
                                        do
                                          localBranchName  @?= "master"
                                          hash             @?= "b7fd5fb0"
                                          rb               @?= Nothing
                                      Nothing                        -> assertFailure "Could not parse as LocalAndRemoteBranch with only local branch"
                                ) parseResult

unit_parseWithLocalAndRemoteBranchWithoutActiveBranch :: Assertion
unit_parseWithLocalAndRemoteBranchWithoutActiveBranch =
  let parseResult = parse localAndRemoteBranch "" "master b7fd5fb0 Merge pull request #1715 from jneira/fix-install-hoogle" in
  either (assertFailure . show) ((@?= Nothing)) parseResult

unit_parseWithCommitsAhead :: Assertion
unit_parseWithCommitsAhead =
  let parseResult = parse commitsAhead "" ": ahead 2]" in
  either (assertFailure . show) (\cAhead ->
                                    case cAhead of
                                      Just (CommitsAhead commits) -> commits @?= 2
                                      Nothing                     -> assertFailure "Could not parse CommitsAhead"
                                ) parseResult

unit_parseWithoutCommitsAhead :: Assertion
unit_parseWithoutCommitsAhead =
  let parseResult = parse (option Nothing commitsAhead) "" "b7fd5fb0" in
  either (assertFailure . show) (\cAhead ->
                                    case cAhead of
                                      Just (CommitsAhead commits) -> assertFailure $ "Expect no commits ahead but got: " <> (show commits)
                                      Nothing                     -> pure ()
                                ) parseResult


-- Issue where the git parser did not handle multiple whitespaces between the local branch name and the git hash
unit_bug30_unit_parseWithLocalAndRemoteBranch :: Assertion
unit_bug30_unit_parseWithLocalAndRemoteBranch =
  let parseResult = parse localAndRemoteBranch "" "* master       3899a59 [origin/master] Cleanup" in
  either (assertFailure . show) (\rBranch ->
                                    case rBranch of
                                      Just (LocalAndRemoteBranch (LocalBranch localBranchName (GitHash hash)) (Just (RemoteBranch remote remoteBranchName ahead))) ->
                                        do
                                          localBranchName  @?= "master"
                                          remote           @?= "origin"
                                          remoteBranchName @?= "master"
                                          hash             @?= "3899a59"
                                          ahead            @?= Nothing
                                      Just other -> assertFailure $ "Could not parse both Local and Remote branches as LocalAndRemoteBranch: " <> (show other)
                                      Nothing    -> assertFailure "Could not parse both Local and Remote branches as LocalAndRemoteBranch"
                                ) parseResult