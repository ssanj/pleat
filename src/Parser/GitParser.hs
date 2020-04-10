{-# LANGUAGE DerivingStrategies #-}

module Parser.GitParser
       (
          -- Data types
          Parser
       ,  LocalBranch(..) 
       ,  RemoteBranch(..) 
       ,  LocalAndRemoteBranch(..) 
          -- Functions
       ,  localBranch
       ,  remoteBranch
       ,  localAndRemoteBranch
       ) where

import qualified Text.Parsec as P
-- import qualified Text.Parsec.Char as P

type Parser = P.Parsec String ()

newtype LocalBranch = LocalBranch String deriving stock (Eq, Show)
data RemoteBranch = RemoteBranch { _remote :: String, _branch :: String } deriving stock (Eq, Show)

data LocalAndRemoteBranch = LocalAndRemoteBranch { _localBranch :: LocalBranch, _remoteBranchMaybe :: Maybe RemoteBranch } deriving stock (Eq, Show)

-- * master b93b0b7 More WIP
localBranch :: Parser (Maybe LocalBranch)
localBranch = (fmap Just parseLocalBranch) P.<|> (pure Nothing)

parseLocalBranch :: Parser LocalBranch
parseLocalBranch = P.try $ do
  _ <- P.char '*'
  _ <- P.space
  branchName <- P.many1 $ P.noneOf "^~ \\"
  pure $ LocalBranch branchName

-- * master b7fd5fb0 [origin/master] Merge pull request #1715 from jneira/fix-install-hoogle
remoteBranch :: Parser (Maybe RemoteBranch)
remoteBranch = (fmap Just parseRemoteBranch) P.<|> (pure Nothing)

localAndRemoteBranch :: Parser (Maybe LocalAndRemoteBranch)
localAndRemoteBranch = (fmap Just parseLocalAndRemoteBranch) P.<|> (pure Nothing)

parseLocalAndRemoteBranch :: Parser LocalAndRemoteBranch
parseLocalAndRemoteBranch = do
  lb      <- parseLocalBranch
  rbMaybe <- remoteBranch
  pure $ LocalAndRemoteBranch lb rbMaybe

parseRemoteBranch :: Parser RemoteBranch
parseRemoteBranch = P.try $ do -- we need the try here because of the manyTill
    _          <- P.manyTill P.anyChar (P.lookAhead $ P.char '[')
    _          <- P.char '['
    remote     <- P.manyTill P.anyChar (P.lookAhead $ P.char '/')
    _          <- P.char '/'
    branchName <- P.manyTill P.anyChar (P.lookAhead $ P.char ']')
    _          <- P.char ']'
    pure $ RemoteBranch remote branchName


