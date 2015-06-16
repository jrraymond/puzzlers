module SudokuGenerator where

import System.Random
import SudokuSolver
import LatinSquare
import Data.Maybe (isJust)


genSudoku :: RandomGen g => g -> Difficulty -> Sudoku
genSudoku g d = let sq = genLatinSquare g 9
                    (n, g') = numHoles g d
                in eraseGivens g' n sq

eraseGivens :: RandomGen g => g -> Int -> Sudoku -> Sudoku
eraseGivens g0 n0 s0 = go g0 n0 occupied s0
  where dim = length s0
        occupied = [ (ri, ci) | ri <- [0 .. dim - 1]
                              , ci <- [0 .. dim - 1] ]
        go :: RandomGen g => g -> Int -> [(Int, Int)] -> Sudoku -> Sudoku
        go _ 0 _ s = s
        go g n is s = let (ix, g') = randomR (0, length is - 1) g
                          (ri, ci) = is !! ix
                          is' = take ix is ++ drop (ix + 1) is
                          s' = modCell (const 0) ri ci s
                      in if isJust (sudoku s) 
                           then go g' (n - 1) is' s'
                           else go g' n is s

numHoles :: RandomGen g => g -> Difficulty -> (Int, g)
numHoles g d = let range = case d of
                             Easiest  -> [50 .. 60]
                             Easy     -> [36 .. 39]
                             Moderate -> [32 .. 35]
                             Hard     -> [28 .. 31]
                             Evil     -> [22 .. 27]
               in randomElem g range
