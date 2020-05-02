module Feature.Path
       (
          -- Functions
          processPath
       ) where

import qualified Api         as A
import qualified Format.Path as PF

import Config

processPath :: Config -> IO String
processPath config = PF.processPath (_maxPathLength config) <$> A.getCurrentDirectory