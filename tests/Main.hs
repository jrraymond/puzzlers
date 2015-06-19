module Main (main) where

import SudokuSolver
import SudokuGenerator
import KenKenGenerator
import KenKenSolver
import LatinSquare
import System.Exit (exitFailure, exitSuccess)
import System.Random

main :: IO ()
main = do let sol = sudoku sEasy
          --print sol
          let eq = sol == Just sEasySol
          let sol2 = sudoku sEasySol
          --print sol2 let (sol3, gen_sudoku) = genSudoku (mkStdGen 0) Easiest
          --print gen_sudoku
          --print sol3
          let kenkensol = kenken (length kkEasy, kkEasy)
          let kenkeneq = kenkensol == Just  kkEasySol
          let kenkensolH = kenken (length kkHard, kkHard)
          let kenkeneqH = kenkensolH == Just kkHardSol
          print kenkensol
          print kenkeneq
          if not eq || not kenkeneq || not kenkeneqH
            then exitFailure
            else exitSuccess

sEasy :: [[Int]]
sEasy = [[5,3,0,0,7,0,0,0,0],
         [6,0,0,1,9,5,0,0,0],
         [0,9,8,0,0,0,0,6,0],
         [8,0,0,0,6,0,0,0,3],
         [4,0,0,8,0,3,0,0,1],
         [7,0,0,0,2,0,0,0,6],
         [0,6,0,0,0,0,2,8,0],
         [0,0,0,4,1,9,0,0,5],
         [0,0,0,0,8,0,0,7,9]]

sEasySol :: [[Int]]
sEasySol = [[5,3,4,6,7,8,9,1,2],
            [6,7,2,1,9,5,3,4,8],
            [1,9,8,3,4,2,5,6,7],
            [8,5,9,7,6,1,4,2,3],
            [4,2,6,8,5,3,7,9,1],
            [7,1,3,9,2,4,8,5,6],
            [9,6,1,5,3,7,2,8,4],
            [2,8,7,4,1,9,6,3,5],
            [3,4,5,2,8,6,1,7,9]]

kkEasy :: [([Int] -> Int, Int, [(Int,Int)])]
kkEasy = [ (divide  , 2, [(0,0),(1,0)])
         , (plus    , 3, [(0,1)])
         , (minus   , 3, [(0,2),(0,3)])
         , (divide  , 2, [(1,1),(2,1)])
         , (plus    , 7, [(1,2),(2,2)])
         , (plus    , 1, [(1,3)])
         , (minus   , 2, [(2,0),(3,0)])
         , (divide  , 2, [(3,1),(3,2)])
         , (multiply, 6, [(2,3),(3,3)]) ]

kkEasySol :: [[Int]]
kkEasySol = [[2,3,1,4]
            ,[4,2,3,1]
            ,[3,1,4,2]
            ,[1,4,2,3]]

kkHard :: [([Int] -> Int, Int, [(Int,Int)])]
kkHard = [ (minus   ,  1, [(0,0),(1,0)])
         , (divide  ,  3, [(0,1),(1,1)])
         , (multiply, 10, [(0,2),(0,3)])
         , (plus    , 11, [(0,4),(0,5),(1,4)])
         , (plus    , 12, [(1,2),(1,3),(2,2)])
         , (minus   ,  1, [(2,0),(3,0)])
         , (minus   ,  1, [(2,1),(3,1)])
         , (minus   ,  3, [(2,3),(2,4)])
         , (plus    , 14, [(1,5),(2,5),(3,5),(4,5)])
         , (multiply,  6, [(4,0),(4,1)])
         , (plus    , 10, [(3,2),(4,2)])
         , (plus    ,  7, [(3,3),(3,4),(4,4)])
         , (plus    ,  3, [(4,3)])
         , (minus   ,  2, [(5,0),(5,1)])
         , (minus   ,  5, [(5,2),(5,3)])
         , (multiply, 15, [(5,4),(5,5)]) ]

kkHardSol :: [[Int]]
kkHardSol = [[3,1,2,5,6,4]
            ,[2,3,5,4,1,6]
            ,[6,5,3,1,4,2]
            ,[5,4,6,2,3,1]
            ,[1,6,4,3,2,5]
            ,[4,2,1,6,5,3]]
