{-# LANGUAGE DerivingStrategies #-}

module Feature.Live.Component.PromptSeparator
       (
          -- Functions
          processPromptSeparator
       ) where

import qualified Config as C

import Config (Config(..))
import Feature.Model (PromptSeparator(..))

processPromptSeparator :: Config -> PromptSeparator
processPromptSeparator = PromptSeparator . C._promptSeparator . _pleatPromptSeparator
