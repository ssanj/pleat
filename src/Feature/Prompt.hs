{-# LANGUAGE DerivingStrategies #-}

module Feature.Prompt
       (
          -- Functions
          processPromptSuffix
       ) where

import qualified Config as C

import Config (Config(..))
import Feature.Model (Prompt(..))

processPromptSuffix :: Config -> Maybe Prompt
processPromptSuffix = Just. Prompt . C._prompt . _pleatPrompt
