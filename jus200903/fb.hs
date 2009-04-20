
fizz,buzz :: [String]
fizzBuzz = zipWith f [0..] (zipWith (++) fizz buzz)
    where
      f n "" = show n
      f _ s  = s

fizz = cycle (take 3 ("Fizz":repeat ""))
buzz = cycle (take 5 ("Buzz":repeat ""))

answer1 = take 100 (tail fizzBuzz)


