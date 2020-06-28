{-# LANGUAGE DerivingStrategies #-}

module Feature.Live.Component.Path
       (
          -- Functions
          processPath
       ) where

import qualified Feature.Live.Internal    as A
import qualified Feature.Live.Format.Path as PF

import Config
import Feature.Model (Path(..))

processPath :: Config -> IO (Maybe Path)
processPath (Config { _pleatPathOption = OptionOff })                           = pure Nothing
processPath (Config { _pleatPathOption = OptionOn (PathOption maxPathLength) }) =
  (Just . Path . PF.processPath maxPathLength) <$> A.getCurrentDirectory