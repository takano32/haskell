#!/usr/bin/env hugs

newtype Negative = MakeNegative Integer deriving(Show, Eq)
toNegative :: Integer -> Negative
toNegative x | x >= 0 = error "Can't create positive number."
             | otherwise = MakeNegative x

fromNegative :: Negative -> Integer
fromNegative (MakeNegative x) = x

instance Num Negative where
    fromInteger = toNegative
    x + y = toNegative (fromNegative x + fromNegative y)
    x - y = let r = fromNegative x - fromNegative y
                    in if r >= 0 then error "No negative substraction"
                       else toNegative r
    x * y = error "Can't positive multiplication"


e1 :: Negative
e1 = toNegative (-2)
e2 :: Negative
e2 = -2
e3 :: Negative
e3 = (-2) + (-7)

e4 ::Negative
e4 = (-2) - (-7)
-- error

e5 :: Negative
e5 = negate 5 -- (0 -x)
-- error



