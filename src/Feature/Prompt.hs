module Feature.Prompt
       (
          -- Data types
          Prompt(..)
          -- Functions
       ,  processPromptSuffix
       ) where

import qualified Config as C

import Config (Config(..))


newtype Prompt = Prompt { _prompt :: String }

processPromptSuffix :: Config -> Maybe Prompt
processPromptSuffix = Just. Prompt . C._prompt . _pleatPrompt
