module Main (main) where

import Criterion.Main
import System.Random
import SudokuSolver
import SudokuGenerator
import LatinSquare

easiestSudokus :: Int -> Sudoku
easiestSudokus = (map (snd . (\i -> genSudoku (mkStdGen i) Easiest)) [0..] !!)

easySudokus :: Int -> Sudoku
easySudokus = (map (snd . (\i -> genSudoku (mkStdGen i) Easy)) [0..] !!)

moderateSudokus :: Int -> Sudoku
moderateSudokus = (map (snd . (\i -> genSudoku (mkStdGen i) Moderate)) [0..] !!)


main :: IO ()
main = defaultMain 
  [
    bgroup "sudoku gen" [ bench "easiest" $ nf (genSudoku (mkStdGen 0)) Easiest
                        , bench "easy" $ nf (genSudoku (mkStdGen 1)) Moderate
                        , bench "moderate" $ nf (genSudoku (mkStdGen 2)) Hard
                        ]
  , bgroup "sudoku solve" [ bench "1" $ nf sudoku (easiestSudokus 0)
                          , bench "2" $ nf sudoku (easySudokus 1)
                          , bench "3" $ nf sudoku (moderateSudokus 2)
                          ]
  ]
