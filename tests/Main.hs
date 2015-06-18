module Main (main) where

import SudokuSolver
import SudokuGenerator
import KenKenGenerator
import KenKenSolver
import LatinSquare
import System.Exit (exitFailure, exitSuccess)
import System.Random

main :: IO ()
main = do let sol = sudoku sudokuPuzzle
          print sol
          let eq = sol == Just sudokuSolution
          let sol2 = sudoku sudokuSolution
          print sol2
          let (sol3, gen_sudoku) = genSudoku (mkStdGen 0) Easiest
          print gen_sudoku
          print sol3
          let kenkensol = kenken (4, kenkenPuzzle)
          let kenkeneq = kenkensol == Just  kenkenSolution
          print kenkeneq
          if not eq || not kenkeneq
            then exitFailure
            else exitSuccess

sudokuPuzzle :: [[Int]]
sudokuPuzzle = [[5,3,0,0,7,0,0,0,0],
          [6,0,0,1,9,5,0,0,0],
          [0,9,8,0,0,0,0,6,0],
          [8,0,0,0,6,0,0,0,3],
          [4,0,0,8,0,3,0,0,1],
          [7,0,0,0,2,0,0,0,6],
          [0,6,0,0,0,0,2,8,0],
          [0,0,0,4,1,9,0,0,5],
          [0,0,0,0,8,0,0,7,9]]

sudokuSolution :: [[Int]]
sudokuSolution = [[5,3,4,6,7,8,9,1,2],
                  [6,7,2,1,9,5,3,4,8],
                  [1,9,8,3,4,2,5,6,7],
                  [8,5,9,7,6,1,4,2,3],
                  [4,2,6,8,5,3,7,9,1],
                  [7,1,3,9,2,4,8,5,6],
                  [9,6,1,5,3,7,2,8,4],
                  [2,8,7,4,1,9,6,3,5],
                  [3,4,5,2,8,6,1,7,9]]

kenkenPuzzle :: [([Int] -> Int, Int, [(Int,Int)])]
kenkenPuzzle = [ (divide  , 2, [(0,0),(1,0)])
               , (plus    , 3, [(0,1)])
               , (minus   , 3, [(0,2),(0,3)])
               , (divide  , 2, [(1,1),(2,1)])
               , (plus    , 7, [(1,2),(2,2)])
               , (plus    , 1, [(1,3)])
               , (minus   , 2, [(2,0),(3,0)])
               , (divide  , 2, [(3,1),(3,2)])
               , (multiply, 6, [(2,3),(3,3)]) ]

kenkenSolution :: [[Int]]
kenkenSolution = [[2,3,1,4]
                 ,[4,2,3,1]
                 ,[3,1,4,2]
                 ,[1,4,2,3]]
