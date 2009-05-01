
import System
import List

main = do args <- getArgs
          cs <- getContents
          putStr $ unlines $ uniq $ sort $ lines cs

uniq :: [String] -> [String]
uniq s = s

