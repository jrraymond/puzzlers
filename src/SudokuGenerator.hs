module SudokuGenerator (genSudoku) where

import System.Random
import SudokuSolver
import LatinSquare


genSudoku :: RandomGen g => g -> Difficulty -> Sudoku
genSudoku g d = let sq = genLatinSquare g 9
                    (n, g') = numHoles g d
                in eraseGivens g' n sq

eraseGivens :: RandomGen g => g -> Int -> Sudoku -> Sudoku
eraseGivens = undefined

numHoles :: RandomGen g => g -> Difficulty -> (Int, g)
numHoles g d = let range = case d of
                             Easiest  -> [50 .. 60]
                             Easy     -> [36 .. 39]
                             Moderate -> [32 .. 35]
                             Hard     -> [28 .. 31]
                             Evil     -> [22 .. 27]
               in randomElem g range
