

main = do cs <- getContents
          putStr . unlines $ map fold $ lines cs

fold :: String -> String
fold ss = join $ splitAt 60 ss
    where
      join (a, "")  = a
      join (a, b)   = a ++ "\n" ++ fold b





