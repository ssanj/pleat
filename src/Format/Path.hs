module Format.Path 
       (
           -- Functions
           processPath
       ) where

import qualified Api                  as A
import qualified Internal.BashColours as Colour

processPath :: Maybe A.CurrentDirectory -> String
processPath = formatPath . getCurrentDirectory 

getCurrentDirectory :: Maybe A.CurrentDirectory  -> String
getCurrentDirectory (Just (A.CurrentDirectory pwd)) = 
  let pathLength = length pwd
  in if pathLength > maxPathLength then
    "..." <> takeR (maxPathLength - 3) pwd
  else
    pwd
getCurrentDirectory Nothing = "-"

formatPath :: String -> String
formatPath = Colour.cyan

-- push to a config object
maxPathLength :: Int
maxPathLength = 40

takeR :: Int -> [a] -> [a]
takeR n xs 
  | n <= 0    = xs
  | otherwise = reverse . take n . reverse $ xs
