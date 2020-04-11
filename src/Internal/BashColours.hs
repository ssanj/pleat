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

cyan :: String -> String
cyan = setColour cyanColour

red :: String -> String
red = setColour redColour

yellow :: String -> String
yellow = setColour yellowColour

cyanColour, redColour, yellowColour :: Int

cyanColour   = 96
redColour    = 31
yellowColour = 33

setColour :: Int -> String -> String
setColour code value
  | code > 0 = "\\[\\e[" <> (show code) <> "m\\]" <> value <> "\\[\\e[0m\\]"
  | otherwise = value