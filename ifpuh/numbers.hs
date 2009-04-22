#!/usr/bin/env hugs



myFloor :: Float -> Float
myFloor x = searchFrom 0
            where searchFrom = decrease . upper . lower
                  lower = until (<= x) decrease
                  upper = until (> x) increase
                  decrease n = n - 1
                  increase n = n + 1


-- gcd :: (Integer, Integer) -> Integer
-- gcd (x,y) = if y == 0 then x else gcd(y, x `mod` y)


{--
newtype MyRational = Rat Integer

mkRat :: (Integer, Integer) -> MyRational
mkRat (x,y) = Rat (u `div` d) (v `div` d)
              where u = (signum y) * x
                    v = abs y
                    d = gcd (u,v)

instance Eq MyRational where
    Rat x y == Rat u v = (x * v) == (v * y)

instance Ord MyRational where
    Rat x y < Rat u v = (x * v) == (y * u)

showRat (Rat x y)
    = if y == 1 then show x else show x ++ "/" ++ show y

instance Num MyRational where
    Rat x y + Rat u v = mkRat (x*v + u*y, y*v)
    Rat x y - Rat u v = mkRat (x*v - u*y, y*v)
    Rat x y * Rat u v = mkRat (x*u, y*v)
    negate (Rat x y)  = mkRat (-x, y)
    fromInteger x     = mkRat (x, 1)


Rat x y / Rat u v
    | u < 0 = mkRat (-x*v, -y*u)
    | u == 0 = error "division by 0"
    | u > 0 = mkRat (x*v, y*u)

--}