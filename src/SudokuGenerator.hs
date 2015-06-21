module SudokuGenerator where

import System.Random
import System.Random.Shuffle
import SudokuSolver
import LatinSquare
import Data.Maybe (isNothing, mapMaybe, fromJust)


genSudoku :: RandomGen g => g -> Difficulty -> ([[Int]], [[Int]])
genSudoku g d = let sq = genLatinSquare g 9 sudokuRules
                    (n, g') = numHoles g d
                in (sq, eraseGivens g' n sq)

eraseGivens :: RandomGen g => g -> Int -> [[Int]] -> [[Int]]
eraseGivens g0 n0 s0 
  | isNothing s1 = error "Could not erase sufficient holes"
  | otherwise = fromJust s1
  where s1 = go g0 n0 occupied s0
        dim = length s0
        occupied = [ (ri, ci) | ri <- [0 .. dim - 1]
                              , ci <- [0 .. dim - 1] ]
        go :: RandomGen g => g -> Int -> [(Int, Int)] -> [[Int]] -> Maybe [[Int]]
        go _ 0 _ s = Just s
        go g n is s 
          | null paths = Nothing
          | otherwise = Just $ head paths
          where
            ixs = shuffle' [0 .. length is - 1] (length is) g
            paths = mapMaybe (\ix -> let is' = take ix is ++ drop (ix + 1) is
                                         (r, c) = is !! ix
                                         s' = modCell (const 0) r c s
                                         solved = sudoku s'
                                     in if solved /= Just s0
                                          then Nothing
                                          else go (snd $ next g) (n - 1) is' s')
                             ixs

numHoles :: RandomGen g => g -> Difficulty -> (Int, g)
numHoles g d = let range = case d of
                             Easiest  -> [21 .. 31]
                             Easy     -> [32 .. 45]
                             Moderate -> [46 .. 49]
                             Hard     -> [50 .. 53]
                             Evil     -> [54 .. 59]
               in randomElem g range
