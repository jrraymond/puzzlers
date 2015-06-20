module Main (main) where

import Criterion.Main
import System.Random
import SudokuSolver
import SudokuGenerator
import LatinSquare

easiestSudokus :: Int -> Sudoku
easiestSudokus = (map (snd . (\i -> genSudoku (mkStdGen i) Easiest)) [0..] !!)

main :: IO ()
main = defaultMain 
  [
    bgroup "sudoku gen" [ bench "1" $ nf (genSudoku (mkStdGen 0)) Easiest
                        , bench "2" $ nf (genSudoku (mkStdGen 1)) Easiest
                        , bench "3" $ nf (genSudoku (mkStdGen 2)) Easiest
                        ]
  , bgroup "sudoku solve" [ bench "1" $ nf sudoku (easiestSudokus 0)
                          , bench "2" $ nf sudoku (easiestSudokus 1)
                          , bench "3" $ nf sudoku (easiestSudokus 2)
                          ]
  ]
