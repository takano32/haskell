#!/usr/bin/env hugs

hello :: IO()
hello = (putStr "hoge\nfuga")

pow :: Float -> Float -> Float
-- pow x y = if y == 0 then x else pow (x*x) (y-1)
pow x y = x**y


main = hello

