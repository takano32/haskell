#!/usr/bin/env hugs

add :: Integer -> Integer -> Integer
add x y = x + y

inc :: Integer -> Integer
inc = add 1


ones = 1 : ones

numsForm n = n : numsForm (n+1)
squares = map (^2) (numsForm 0)
-- takeWhile (< 20) squares



fib = 1 : 1 : [ a+b | (a,b) <- zip fib (tail fib) ]

