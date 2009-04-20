

fact :: Integer -> Integer
fact 1 = 1
fact n = fact (n - 1) * n
-- 定義の順番に注意する