module Commands.Git 
(
  -- functions
     branch
  ,  branchExtraVerbose
  ,  shortStatus
) where

import Commands.Model (CommandDescription(..), Command(..), Args(..))

branch :: CommandDescription
branch = CommandDescription (Command "git") (Args ["branch"])

branchExtraVerbose :: CommandDescription
branchExtraVerbose = CommandDescription (Command "git") (Args ["branch", "-vv"])

shortStatus :: CommandDescription
shortStatus = CommandDescription (Command "git") (Args ["status", "--short"])