
import System
import List

main = do args <- getArgs
          cs <- getContents
          putStr $ unlines $ sort $ lines cs

