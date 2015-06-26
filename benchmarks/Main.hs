module Main (main) where

import Criterion.Main
import System.Random
import System.Environment (getArgs, withArgs)

import ParseArgs
import SudokuSolver
import SudokuGenerator
import KenKenSolver
import KenKenGenerator
import LatinSquare


main :: IO ()
main = do (o,n) <- parseOpts =<< getArgs
          withArgs n $ defaultMain [getBench o]



suGs :: [Benchmark]
suGs = [ bench "sudoku gen easiest"  $ nf (genSudoku (mkStdGen 0)) Easiest
       , bench "sudoku gen easy"     $ nf (genSudoku (mkStdGen 0)) Easy
       , bench "sudoku gen moderate" $ nf (genSudoku (mkStdGen 0)) Moderate
       , bench "sudoku gen hard"     $ nf (genSudoku (mkStdGen 0)) Hard
       , bench "sudoku gen evil"     $ nf (genSudoku (mkStdGen 0)) Evil
       ]
suSs :: [Benchmark]
suSs = [ bench "sudoku solve easiest"  $ nf sudoku (sudokus 0 0)
       , bench "sudoku solve easy"     $ nf sudoku (sudokus 1 0)
       , bench "sudoku solve moderate" $ nf sudoku (sudokus 2 0)
       , bench "sudoku solve hard"     $ nf sudoku (sudokus 3 0)
       , bench "sudoku solve evil"     $ nf sudoku (sudokus 4 0)
       ]
kkGs :: [Benchmark]
kkGs = [ bench "kenken gen 4x4" $ nf (genKenKen (mkStdGen 0)) 4
       , bench "kenken gen 6x6" $ nf (genKenKen (mkStdGen 0)) 6
       ]
kkSs :: [Benchmark]
kkSs = [ bench "kenken solve 4x4" $ nf (kenken 1) (mapFst length $ kenkens 4 0)
       , bench "kenken solve 6x6" $ nf (kenken 1) (mapFst length $ kenkens 6 0)
       ]

getBench :: [Flag] -> Benchmark
getBench [] = bgroup "running all" $ suGs ++ suSs ++ kkGs ++ kkSs
getBench fs 
  | Sudoku `elem` fs && Generate `elem` fs && i < 0 = bgroup "sudoku generate all" suGs
  | Sudoku `elem` fs && Generate `elem` fs = suGs !! i
  | Sudoku `elem` fs && i < 0 = bgroup "sudoku solve all" suSs
  | Sudoku `elem` fs = suSs !! i
  | KenKen `elem` fs && Generate `elem` fs && i < 0 = bgroup "kenken gen all" kkGs
  | KenKen `elem` fs && Generate `elem` fs = kkGs !! (min i (length kkGs - 1))
  | KenKen `elem` fs && i < 0 = bgroup "kenken solve all" kkSs
  | KenKen `elem` fs = kkSs !! (min i (length kkSs - 1))
  | otherwise = error "incorrect flags"
  where i = getDiff fs


sudokus :: Int -> Int -> [[Char]]
sudokus d i = snd (genSudoku (mkStdGen i) (difficulties !! d))

kenkens :: Int -> Int -> ([[Int]], [(Op, Int, [(Int,Int)])])
kenkens d i = genKenKen (mkStdGen i) d
