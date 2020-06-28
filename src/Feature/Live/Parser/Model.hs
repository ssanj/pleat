{-# LANGUAGE DerivingStrategies #-}

module Feature.Live.Parser.Model
       (
          -- Data types
          Parser
       ,  LocalBranch(..)
       ,  RemoteBranch(..)
       ,  LocalAndRemoteBranch(..)
       ,  CommitsAhead(..)
       ,  GitHash(..)
       ) where

import qualified Text.Parsec as P

type Parser = P.Parsec String ()

newtype GitHash = GitHash String deriving stock (Eq, Show)

data LocalBranch = LocalBranch { _local :: String, _hash :: GitHash} deriving stock (Eq, Show)

newtype CommitsAhead = CommitsAhead Int deriving stock (Eq, Show)

data RemoteBranch = RemoteBranch { _remote :: String, _branch :: String, _commitsAhead :: Maybe CommitsAhead } deriving stock (Eq, Show)

data LocalAndRemoteBranch = LocalAndRemoteBranch { _localBranch :: LocalBranch, _remoteBranchMaybe :: Maybe RemoteBranch } deriving stock (Eq, Show)
