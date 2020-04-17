module Main where

import Lib (prompt)
import Options.Applicative (execParser)
import Config (Config(..))
import CommandlineOptions (pleatInfo)

main :: IO ()
main = execParser pleatInfo >>= prompt >>= putStrLn  --prompt defaultConfig >>= putStrLn
