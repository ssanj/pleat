module Feature.Prompt
       (
          -- Functions
          processPromptSuffix
       ) where

import Config

processPromptSuffix :: Config -> String
processPromptSuffix = _prompt . _pleatPrompt
