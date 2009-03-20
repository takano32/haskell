

module Main where

import qualified System.IO.UTF8 as U

main :: IO()
main = U.readFile "session.txt" >>= U.putStr


