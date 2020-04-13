module Main where

import Lib (prompt)
import Config (defaultConfig)

main :: IO ()
main = prompt defaultConfig >>= putStrLn
