#!/usr/bin/env hugs

type Board = [[Int]]
type Shot = Int

type Position = (Int, Int)

nextBoards :: (Board, [Position]) -> [(Board, [Position])]

nextBoards (b, s) = [(b, [targets|targets <- nextTargets b])]

nextBoard :: Board -> Position -> Board
nextBoard b (i, j) = ls0 ++ (xs0 ++ x+1:xs1) : ls1
    where (ls0, l:ls1) = splitAt i b
          (xs0, x:xs1) = splitAt j l

nextTargets :: Board -> [Position]
nextTargets b = filter (/= (-1, -1) ) 
                $ map target [(i,j) | i <- [0..5], j <- [0..5]]
                where target (i,j) = if b !! i !! j /= 0
				                     then (i,j)
									 else (-1, -1)

--  hoge = [(x,y) | x <- [0..5], y <- [0..5]]

type Process = (Board, [Position])

solve :: Board -> Process
solve b = (b, [(0,0)])

 
sample :: Board
sample = [[0, 0, 0, 0, 3, 0],
          [0, 0, 0, 0, 0, 0],
          [0, 0, 0, 0, 0, 0],
          [0, 0, 0, 0, 0, 0],
          [0, 0, 0, 0, 0, 0],
          [0, 0, 0, 0, 0, 1]]

