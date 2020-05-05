module Feature.Path
       (
          -- Data types
          Path(..)
          -- Functions
       ,  processPath
       ) where

import qualified Api         as A
import qualified Format.Path as PF

import Config

newtype Path = Path { _path :: String }

processPath :: Config -> IO (Maybe Path)
processPath (Config { _pleatPathOption = OptionOff })                           = pure Nothing
processPath (Config { _pleatPathOption = OptionOn (PathOption maxPathLength) }) = 
  (Just . Path . PF.processPath maxPathLength) <$> A.getCurrentDirectory