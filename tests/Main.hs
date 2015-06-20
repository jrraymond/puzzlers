module Main (main) where

import System.Random
import LatinSquare
import SudokuSolver
import SudokuGenerator
import KenKenGenerator
import KenKenSolver
import Debug.Trace
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import qualified Test.HUnit as HU

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ testGroup "Sudoku Generating" (hUnitTestToTests sudoku_gen_tests)
        , testGroup "Sudoku Solving" (hUnitTestToTests sudoku_solving_tests)
        , testGroup "KenKen Generating" (hUnitTestToTests kenken_gen_tests)
        , testGroup "KenKen Solving" (hUnitTestToTests kenken_solving_tests)
        ]

sudoku_gen_tests :: HU.Test
sudoku_gen_tests = HU.TestList [sudoku_gen_easiest, sudoku_gen_easy, sudoku_gen_moderate, sudoku_gen_hard]

sudoku_gen_easiest :: HU.Test
sudoku_gen_easiest = let (sol, s) = genSudoku (mkStdGen 0) Easiest
                         --solved = trace (show sol ++ "\n\n" ++ show s) $ sudoku s
                         solved = sudoku s
                     in HU.TestCase (HU.assertEqual "easiest" (Just sol) solved)


sudoku_gen_easy :: HU.Test
sudoku_gen_easy = let (sol, s) = genSudoku (mkStdGen 0) Easy
                      --solved = trace (show sol ++ "\n\n" ++ show s) $ sudoku s
                      solved = sudoku s
                  in HU.TestCase (HU.assertEqual "easy" (Just sol) solved)

sudoku_gen_moderate :: HU.Test
sudoku_gen_moderate = let (sol, s) = genSudoku (mkStdGen 0) Moderate
                          --solved = trace (show sol ++ "\n\n" ++ show s) $ sudoku s
                          solved = sudoku s
                      in HU.TestCase (HU.assertEqual "moderate" (Just sol) solved)

sudoku_gen_hard :: HU.Test
sudoku_gen_hard = let (sol, s) = genSudoku (mkStdGen 0) Hard
                      solved = trace (show sol ++ "\n\n" ++ show s) $ sudoku s
                  in HU.TestCase (HU.assertEqual "hard" (Just sol) solved)

kenken_gen_tests :: HU.Test
kenken_gen_tests = HU.TestList [kenken_gen_easiest, kenken_gen_evil]

kenken_gen_easiest :: HU.Test
kenken_gen_easiest = let (sol, s) = genKenKen (mkStdGen 0) Easiest 4
                         solved = kenken (4,s)
                         in HU.TestCase (HU.assertEqual "kenken gen easiest"
                                                        (Just sol)
                                                        solved)

kenken_gen_evil :: HU.Test
kenken_gen_evil = let (sol, s) = genKenKen (mkStdGen 0) Evil 4
                      solved = kenken (4,s)
                  in HU.TestCase (HU.assertEqual "kenken gen evil"
                                                 (Just sol)
                                                 solved)

                                                  
sudoku_solving_tests :: HU.Test
sudoku_solving_tests = HU.TestList [sudoku_easy]

sudoku_easy :: HU.Test
sudoku_easy = HU.TestCase (HU.assertEqual "sudoku easy" 
                                          (Just sEasySol) 
                                          (sudoku sEasy))

kenken_solving_tests :: HU.Test
kenken_solving_tests = HU.TestList [kenken_easy, kenken_hard]

kenken_easy :: HU.Test
kenken_easy = HU.TestCase (HU.assertEqual "kenken easy"
                                          (Just kkEasySol)
                                          (kenken (length kkEasySol,kkEasy)))

kenken_hard :: HU.Test
kenken_hard = HU.TestCase (HU.assertEqual "kenken hard"
                                          (Just kkHardSol)
                                          (kenken (length kkHardSol, kkHard)))



--          --print sol2 let (sol3, gen_sudoku) = genSudoku (mkStdGen 0) Easiest
--          --print gen_sudoku

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

kkEasy :: [(Op, Int, [(Int,Int)])]
kkEasy = [ (Divide  , 2, [(0,0),(1,0)])
         , (Plus    , 3, [(0,1)])
         , (Minus   , 3, [(0,2),(0,3)])
         , (Divide  , 2, [(1,1),(2,1)])
         , (Plus    , 7, [(1,2),(2,2)])
         , (Plus    , 1, [(1,3)])
         , (Minus   , 2, [(2,0),(3,0)])
         , (Divide  , 2, [(3,1),(3,2)])
         , (Multiply, 6, [(2,3),(3,3)]) ]

kkEasySol :: [[Int]]
kkEasySol = [[2,3,1,4]
            ,[4,2,3,1]
            ,[3,1,4,2]
            ,[1,4,2,3]]

kkHard :: [(Op, Int, [(Int,Int)])]
kkHard = [ (Minus   ,  1, [(0,0),(1,0)])
         , (Divide  ,  3, [(0,1),(1,1)])
         , (Multiply, 10, [(0,2),(0,3)])
         , (Plus    , 11, [(0,4),(0,5),(1,4)])
         , (Plus    , 12, [(1,2),(1,3),(2,2)])
         , (Minus   ,  1, [(2,0),(3,0)])
         , (Minus   ,  1, [(2,1),(3,1)])
         , (Minus   ,  3, [(2,3),(2,4)])
         , (Plus    , 14, [(1,5),(2,5),(3,5),(4,5)])
         , (Multiply,  6, [(4,0),(4,1)])
         , (Plus    , 10, [(3,2),(4,2)])
         , (Plus    ,  7, [(3,3),(3,4),(4,4)])
         , (Plus    ,  3, [(4,3)])
         , (Minus   ,  2, [(5,0),(5,1)])
         , (Minus   ,  5, [(5,2),(5,3)])
         , (Multiply, 15, [(5,4),(5,5)]) ]

kkHardSol :: [[Int]]
kkHardSol = [[3,1,2,5,6,4]
            ,[2,3,5,4,1,6]
            ,[6,5,3,1,4,2]
            ,[5,4,6,2,3,1]
            ,[1,6,4,3,2,5]
            ,[4,2,1,6,5,3]]
