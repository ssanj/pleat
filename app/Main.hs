module Main where

import Pleat

main :: IO ()
main = parseArguments >>= prompt >>= putStrLn
