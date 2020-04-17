{-# LANGUAGE ScopedTypeVariables #-}

module Internal.BashColours
       (
          -- Functions
          cyan
       ,  red
       ,  yellow
       ) where

-- TODO: This is a very specific implementation around Bash
--       with only the colours I use. It could be generalised
--       more.

import Data.String (IsString(..))

cyan :: (IsString a, Semigroup a) => a -> a
cyan = setColour cyanColour

red :: (IsString a, Semigroup a) => a -> a
red = setColour redColour

yellow :: (IsString a, Semigroup a) => a -> a
yellow = setColour yellowColour

cyanColour, redColour, yellowColour :: Int

cyanColour   = 96
redColour    = 31
yellowColour = 33

setColour :: (IsString a, Semigroup a) => Int -> a -> a
setColour code value
  | code > 0 = (fromString $ "\\[\\e[" <> (show code) <> "m\\]") <> value <> (fromString "\\[\\e[0m\\]")
  | otherwise = value