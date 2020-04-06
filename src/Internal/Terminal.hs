module Internal.Terminal
       (
         -- Functions
         gitStatusShort
       , gitBranchVerbose
       , gitBranch
       ) where

import System.Exit (ExitCode(..))

import qualified System.Process as P

newtype Command = Command String
newtype Args = Args [String]

gitStatusShort :: IO (Maybe [String])
gitStatusShort = runCommand (Command "git") (Args ["status", "--short"])

gitBranchVerbose :: IO (Maybe [String])
gitBranchVerbose = runCommand (Command "git") (Args ["branch", "-vv"])

gitBranch :: IO (Maybe [String])
gitBranch = runCommand (Command "git") (Args ["branch"])

runCommand :: Command -> Args -> IO (Maybe [String])
runCommand (Command command) (Args args) = do
  (exitCode, stdout, _) <- P.readProcessWithExitCode command args ""
  case exitCode of
    ExitSuccess     -> pure $ Just $ lines stdout
    (ExitFailure _) -> pure Nothing
