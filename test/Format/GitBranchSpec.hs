{-# LANGUAGE OverloadedStrings #-}

module Format.GitBranchSpec where

import qualified Parser.GitParser as Git

import Test.Tasty.HUnit                ((@?=), Assertion)

import Format.GitBranch

unit_processGitBranchWithEmptyInput :: Assertion
unit_processGitBranchWithEmptyInput = processGitBranch (Just []) @?= (Just InitialCommit)

unit_processGitBranchWithNoInput :: Assertion
unit_processGitBranchWithNoInput = processGitBranch Nothing @?= Nothing

unit_processGitBranchWithRemoteBranch :: Assertion
unit_processGitBranchWithRemoteBranch = 
  processGitBranch (Just [
                            "2 3d5ae73 Release 0.0.9-b01"
                          , "add-macro-support 4cf335e [origin/add-macro-support] Choose Exception syntax"
                          , "alternate-exception-syntax d7a99e8 [origin/alternate-exception-syntax] Alternate Exception Syntax"
                          , "* do-something-else 3d5ae73 [moon/do-something-else-remotely] Release 0.0.9-b01"
                          , "composite-assertions 6757633 [origin/composite-assertions] Add travis"
                         ]) @?= Just (LocalAndRemote (Git.LocalAndRemoteBranch (Git.LocalBranch "do-something-else" $ Git.GitHash "3d5ae73") (Just (Git.RemoteBranch "moon" "do-something-else-remotely" Nothing)) ))

unit_processGitBranchWithLocalBranch :: Assertion
unit_processGitBranchWithLocalBranch = 
  processGitBranch (Just ["* slave 99d80d9f Merge pull request #1827 from scala-steward/update/sbt-1.3.9"]) 
    @?= Just (LocalAndRemote (Git.LocalAndRemoteBranch (Git.LocalBranch "slave"  $ Git.GitHash "99d80d9f") Nothing))

unit_processGitBranchWithUnparsableInput :: Assertion
unit_processGitBranchWithUnparsableInput = processGitBranch (Just [""]) @?= Nothing
