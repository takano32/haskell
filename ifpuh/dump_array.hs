#!/usr/bin/env hugs

dumpArray :: [[Integer]] -> String
dumpArray (x:xs) = if length xs == 0 then show x
                   else show x ++ "," ++ dumpArray(xs)




