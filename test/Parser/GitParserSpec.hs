{-# LANGUAGE OverloadedStrings #-}

module Parser.GitParserSpec where

import Test.Tasty.HUnit                (Assertion, assertFailure, (@?=))
import Parser.GitParser                (LocalBranch(..), RemoteBranch(..), LocalAndRemoteBranch(..), CommitsAhead(..), localBranch, remoteBranch, localAndRemoteBranch, commitsAhead)
import Text.Parsec                     (parse, option)

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
                                      Just (RemoteBranch remote branchName _) -> 
                                        do 
                                          remote     @?= "origin" 
                                          branchName @?= "master"
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
                                      Just (LocalAndRemoteBranch (LocalBranch localBranchName) (Just (RemoteBranch remote remoteBranchName ahead))) -> 
                                        do 
                                          localBranchName  @?= "slave"
                                          remote           @?= "origin" 
                                          remoteBranchName @?= "master"
                                          ahead            @?= Nothing
                                      Just other -> assertFailure $ "Could not parse both Local and Remote branches as LocalAndRemoteBranch: " <> (show other)
                                      Nothing    -> assertFailure "Could not parse both Local and Remote branches as LocalAndRemoteBranch"
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

