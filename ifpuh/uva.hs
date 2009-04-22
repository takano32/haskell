#! /usr/bin/env hugs
-- http://online-judge.uva.es/p/v1/100.html


cnt :: Integer -> (Integer -> Integer)
{-
inc :: Integer -> Integer
inc n = if (n `mod` 2 == 1) then (3 * n + 1)
            else n `div` 2

cnt n count = if (1 == n) then count
                else cnt (inc n) (count + 1)
-}
cnt n i = if 1 == n then i
          else if n `mod` 2 == 1 then (cnt (3 * n + 1) (i + 1))
          else (cnt (n `div` 2) (i + 1))

uva :: Integer -> (Integer -> Integer)
uva i j = if i > j then 0 else
            max (cnt i 1) (uva (i+1) j)

