module Main where

import Lib (prompt)

main :: IO ()
main = prompt >>= putStrLn
