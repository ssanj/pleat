module Main where

import Lib (prompt)
import Commandline.CommandlineOptions (parseArguments)

main :: IO ()
main = parseArguments >>= prompt >>= putStrLn
