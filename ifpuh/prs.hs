#!/usr/bin/env hugs
import Random

data Move = Paper | Rock | Scissors
type Round = (Move, Move)

score :: Round -> (Int, Int)
score (x, y)
    | x `beats` y = (1, 0)
    | y `beats` x = (0, 1)
    | otherwise = (0, 0)

beats :: Move -> Move -> Bool
beats x y = (m+1 == n) || (m == n+2)
    where m = code x; n = code y

code :: Move -> Int
code Paper = 0
code Rock = 1
code Scissors = 2


pair :: (a -> b, a -> c) -> a -> (b, c)
pair (f,g) x = (f x, g x)


match :: Int -> (Strategy, Strategy) -> (Int, Int)
match n = total . map score . take n . rounds
total :: [(Int, Int)] -> (Int, Int)
total = pair (sum . map fst, sum . map snd)


type Strategy = [Move] -> Move

myRecip :: Strategy
myRecip ms = if null ms then Rock else last ms

smart :: Strategy
smart ms = if null ms then Rock else choose (count ms)


count :: [Move] -> (Int, Int, Int)
count = foldl `oplus` (0, 0, 0)

oplus :: (Int, Int, Int) -> Move -> (Int, Int, Int)
oplus (p, r, s) Paper = (p+1, r, s)
oplus (p, r, s) Rock = (p, r+1, s)
oplus (p, r, s) Scissors = (p, r, s+1)

choose :: (Int, Int, Int) -> Move
choose (p, r, s)
    | (fst m) < p = Scissors
    | (fst m) < p + r = Paper
    | otherwise = Rock
    where m = myRandom p+r+s

myRandom :: Int
myRandom = getStdRandom (randomR (1, 6)) :: IO Int
-- getStdRandom (randomR (1, 6)) :: IO Int


rounds :: (Strategy, Strategy) -> [Round]
rounds (f, g) = (map last . tail . iterate (extend (f, g))) []

extend :: (Strategy, Strategy) -> [Round] -> [Round]
extend (f, g) rs = rs ++ [(f(map snd rs), g(map fst rs))]


{--
randoms :: [Int]
randoms = iterate f seed
    where f x = (a * x + c) mod m
--}
