


stepWays :: Integer -> Integer

stepWays 0 = 1
stepWays 1 = stepWays 0
stepWays n = stepWays (n - 1) + stepWays (n - 2)

stepWays2 :: Integer -> (Integer, Integer)
stepWays2 0 = (1, 1)       -- (stepWays 0, stepWays 1)
stepWays2 n = (b, a + b)
    where (a, b) = stepWays2 (n - 1)

fastStepWays :: Integer -> Integer
fastStepWays = fst . stepWays2

fastStepWays2 :: Integer -> (Integer, Integer)
fastStepWays2 0 = (1, 1)
fastStepWays2 n | even n    = (c, d)
                | otherwise = (d, c + d)
    where (c, d) = (a^2 + (b-a)^2, 2 * (2*b -a ))
          (a, b) = fastStepWays2 (n `div` 2)




