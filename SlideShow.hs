

module Main where

import qualified System.IO.UTF8 as U

main :: IO()
main = U.getContents >>= U.putStr

