#!/usr/bin/env hugs

data DumbTree = Empty | Fork DumbTree DumbTree | LeftEmpty | RightEmpty

-- 整数から木のリスト
trees, leftTrees, rightTrees :: Int -> [DumbTree]
leftTrees 1 = [LeftEmpty]
leftTrees n = trees n
rightTrees 1 = [RightEmpty]
rightTrees n = trees n
trees 1 = [Empty]
trees n = concat [joins ls rs | (ls,rs) <- [lrs xs ys | (xs,ys) <- splits1 n] ]




-- 整数から和がその数になる整数のふたつ組みのリスト
splits1 :: Int -> [(Int,Int)]
splits1 1 = []
splits1 n = (1,n-1) : [(i+1, j) | (i,j) <- splits1 (n-1)]

lrs :: Int -> Int -> ([DumbTree], [DumbTree])
lrs xs ys = (leftTrees xs, rightTrees ys)

joins :: [DumbTree] -> [DumbTree] -> [DumbTree]
joins ls rs = [ Fork l r | l <- ls, r <- rs ]

instance Show DumbTree where
    show Empty = "0"
    show LeftEmpty = "L"
    show RightEmpty = "R"
    show (Fork l r) = "(" ++ show l ++ "^" ++ show r ++ ")"

