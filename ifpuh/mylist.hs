#!/usr/bin/env hugs

-- qs :: Ord a => [a] -> [a]
qs [x] = [x]
qs (x:xs) = (qs (less x xs)) ++ [x] ++
            (qs (greater x xs))

less x [] = []
less x (y:ys) = if (<=) x y then (less x ys) 
              else [y] ++ (less x ys)

greater x [] = []
greater x (y:ys) = if (>) x y then (greater x ys)
                 else [y] ++ (greater x ys)

-- aMultiply :: [[a]] -> [[a]] -> [a]
aMultiply (xs:xss) (ys:yss)
    = if null ys then [] else
          sum(zipWith (*) xs (map head (ys:yss)))
                 : (aMultiply (xs:xss) (map tail (ys:yss)))
multiply [] _ = []
multiply (xs:xss) (ys:yss)
    = if null xs then [] else
          (aMultiply (xs:xss) (ys:yss)) : (multiply xss (ys:yss))

