

main = do cs <- getContents
          print $ length $ firstNLines 10 cs

firstNLines n cs = unlines $ take n $ lines cs





