module Format.Path 
       (
           -- Functions
           processPath
       ) where

import qualified Api                  as A
import qualified Internal.BashColours as Colour

import Config (MaxPathLength(..))

processPath :: MaxPathLength -> Maybe A.CurrentDirectory -> String
processPath maxPathLength currentDir = formatPath $ getCurrentDirectory maxPathLength currentDir

getCurrentDirectory :: MaxPathLength -> Maybe A.CurrentDirectory  -> String
getCurrentDirectory (MaxPathLength maxPathLength) (Just (A.CurrentDirectory pwd)) = 
  let pathLength = length pwd
  in if pathLength > maxPathLength then
    "..." <> takeR (maxPathLength - 3) pwd
  else
    pwd
getCurrentDirectory _ Nothing = "-"

formatPath :: String -> String
formatPath = Colour.cyan

takeR :: Int -> [a] -> [a]
takeR n xs 
  | n <= 0    = xs
  | otherwise = reverse . take n . reverse $ xs
