#!/usr/bin/env hugs

square :: Integer -> Integer
square x = x * x

smaller :: (Integer, Integer) -> Integer
smaller (x, y) = if x <= y then x else y


-- square :: Float -> Float
-- square x = x * x

-- delta :: (Float, Float, Float) -> Float
-- delta(a, b, c) = sqrt(square b - 4 * a * c)


three :: Integer -> Integer
three x = 3

infinity :: Integer
infinity = infinity + 1


double, double' :: Integer -> Integer
-- double' :: Integer -> Integer
double x = x + x
double' x = 2 * x

smallerc :: Integer -> (Integer -> Integer)
smallerc x y = if x <= y then x else y

plus :: (Integer, Integer) -> Integer
plus (x, y) = x + y

plusc :: Integer -> (Integer -> Integer)
plusc x y = x + y -- means (plusc x) y



multiply :: (Integer, Integer) -> Integer
multiply(x, y) = if x == 0 then 0 else x * y

-- twice :: (Integer -> Integer) -> (Integer -> Integer)
-- twice f x = f (f x)

-- quad :: Integer -> Integer
-- quad = twice square


twice :: (Integer -> Integer, Integer) -> Integer
twice (f, x) = f (f x)

quad :: Integer -> Integer
quad x = twice (square, x)



-- (AxB -> C) == (A -> (B -> C))
-- ((A, B) -> C)  (A -> (B -> C))  (A -> B -> C)



signum :: Integer -> Integer
signum x = if x < 0 then - 1 else
		if x == 0 then 0 else 1

fact :: Integer -> Integer
fact x = if x == 0 then 1 else
		if x < 0 then error "negative argument to fact!"
		else x * (fact (x - 1))



