#!/usr/bin/env hugs
-- solve sample
-- sampleの要素
import List
-- 小さいマスのサイズ
sudokuBase :: Int

-- 大きいマスのサイズ
sudokuSize :: Int

sudokuBase = 3
sudokuSize = sudokuBase ^ 2

type Sudoku = Int
sudokuElm :: [Sudoku]
sudokuElm = [1..sudokuSize]
vacant :: Sudoku
vacant = 0

type SudokuBoard = [[Sudoku]]
sample :: SudokuBoard
sample = [[8, 0, 0, 0, 3, 4, 0, 5, 0],
          [0, 0, 2, 0, 0, 0, 0, 0, 1],
          [0, 1, 0, 9, 0, 0, 0, 0, 0],
          [0, 0, 8, 0, 0, 9, 0, 0, 6],
          [5, 0, 0, 0, 1, 0, 0, 0, 8],
          [6, 0, 0, 4, 0, 0, 7, 0, 0],
          [0, 0, 0, 0, 0, 1, 0, 7, 0],
          [2, 0, 0, 0, 0, 0, 1, 0, 0],
          [0, 9, 0, 5, 6, 0, 0, 0, 2]]

-- (列, 行)
type Position = (Int, Int)

sudoku :: SudokuBoard -> [SudokuBoard]
sudoku b
    = case vacantPositions b of 
        [] -> [b]
        ps -> case nextVacant b ps of
                (p, xs) -> 
                    concatMap sudoku
                    $ map (putCell b)
                    $ [(p, x) | x <- xs]

vacantPositions :: SudokuBoard -> [Position]
nextVacant :: SudokuBoard -> [Position] -> (Position, [Sudoku])
putCell :: SudokuBoard -> (Position, Sudoku) -> SudokuBoard
                
-- vacantPositions = error "(vacantPositions) is not yet implemented"
-- nextVacant = error "(nextVacant) is not yet implemented"
-- putCell = error "(putCell) is not yet implemented"

putCell b ((i,j), x) = ls0 ++ (xs0 ++ x:xs1) : ls1
    where (ls0, l:ls1) = splitAt j b
          (xs0, _:xs1) = splitAt i l

nextVacant b ps = minimumBy cmp [(p,candidate b p) | p <- ps]
    where (_,xs) `cmp` (_,ys) = length xs `compare` length ys
-- nextVacant b ps = (head ps, [])
--    where (x,xs) `cmp` (y,ys) = if length xs < length ys then (x,xs) else (y,ys)

candidate :: SudokuBoard -> Position -> [Sudoku]
candidate b p = sudokuElm \\ nub (concat $ map (\c -> c b p) [col, row, box])

col, row, box :: SudokuBoard -> Position -> [Sudoku]
col b (i,_) = transpose b !! i
row b (_,j) = b !! j
box b (i,j) = concat
              $ map (take sudokuBase) $ map (drop $ (i `div` sudokuBase) * sudokuBase)
              $ take sudokuBase $ drop ((j `div` sudokuBase) * sudokuBase) b

vacantPositions b = map fst
                    $ filter (\ (p, x) -> vacant == x)
                    $ concatMap (\ (j,xs) -> zipWith (\ i x -> ((i,j),x)) [0..] xs)
                    $ zip [0..] b

-- usage: nextVacant sample (vacantPositions sample)
-- usage: sudoku sample


showSudokuBoard :: SudokuBoard -> String
showSudokuBoard b = unlines $ map (unwords . map show) b

readSudokuBoard :: String -> SudokuBoard
readSudokuBoard s = map (map read) $ map words $ lines s

-- usage: (readSudokuBoard . showSudokuBoard) sample

{-
ghci -fglasgow-exts -fallow-overlapping-instances
instance Show SudokuBoard where
    show = showSudokuBoard

instance Read SudokuBoard where
    readsPrec _ str = [(readSudokuBoard str, "")]
-}



{-
i (h (g (f x)))
i $ h $ g $ f x
 ($) f x = f x 

Main> (+3) $ 4
7

Main> [0,1,2,3,4,7] \\ [2,4,5,6]
[0,1,3,7]

Main> (\c -> 1:2:c) [3,4]
[1,2,3,4]


Main> nub [0,1,2,3,3]
[0,1,2,3]
-}

-- ($)
dollar :: (a -> b) -> a -> b
dollar f x = f x

mynull :: [a] -> Bool
mynull x = case (x) of
           ([]) -> True
           (_) -> False

sample2 :: SudokuBoard
sample2 = [[0, 0, 0, 9, 1, 8, 0, 0, 0],
           [0, 0, 3, 0, 0, 0, 2, 0, 0],
           [0, 5, 6, 0, 0, 0, 1, 4, 0],
           [3, 0, 0, 2, 0, 9, 0, 0, 7], 
           [4, 0, 0, 0, 8, 0, 0, 0, 5], 
           [7, 0, 0, 3, 5, 4, 0, 0, 2], 
           [0, 1, 4, 0, 9, 0, 7, 5, 0], 
           [0, 0, 0, 0, 7, 0, 0, 0, 0], 
           [0, 0, 8, 6, 3, 1, 9, 0, 0]]
