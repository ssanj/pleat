{-# LANGUAGE DerivingStrategies #-}

module Feature.PromptSeparator
       (
          -- Data types
          PromptSeparator(..)
          -- Functions
       ,  processPromptSeparator
       ) where

import qualified Config as C

import Config (Config(..))


newtype PromptSeparator = PromptSeparator { _promptSeparator :: String } deriving stock (Eq, Show)

processPromptSeparator :: Config -> PromptSeparator
processPromptSeparator = PromptSeparator . C._promptSeparator . _pleatPromptSeparator
