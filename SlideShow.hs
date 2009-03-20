

module Main where

import qualified System.IO.UTF8 as U

main :: IO()
main = getArgs >>= U.readFile . head >>= U.putStr


