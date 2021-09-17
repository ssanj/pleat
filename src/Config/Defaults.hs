{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

module Config.Defaults
       (
           -- Functions
           defaultMaxPathLength
        ,  defaultPrompt
        ,  defaultPromptSeparator
        ) where

import Config.Model (MaxPathLength(..), Prompt(..), PromptSeparator(..))

defaultMaxPathLength :: MaxPathLength
defaultMaxPathLength = MaxPathLength 50

defaultPrompt :: Prompt
defaultPrompt = Prompt "> "

defaultPromptSeparator :: PromptSeparator
defaultPromptSeparator = PromptSeparator ":"
