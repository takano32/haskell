
main = do cs <- getContents
          putStr $ expand cs

expand :: String -> String
expand cs = map swapa cs

swapa :: Char -> Char
swapa 'a' = 'A'
swapa 'A' = 'a'
swapa  c  =  c

