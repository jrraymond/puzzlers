module SudokuGenerator where

import System.Random
import System.Random.Shuffle
import Data.Maybe (isNothing, mapMaybe, fromJust)
import Data.Either (isLeft)
import Data.Char (intToDigit)


import SudokuSolver
import LatinSquare


genSudoku :: RandomGen g => g -> Difficulty -> ([[Char]], [[Char]])
genSudoku g d = let sq = intsToChars $ genLatinSquare g 9 sudokuRules
                    (n, g') = numHoles g d
                in (sq, eraseGivens g' n sq)

eraseGivens :: RandomGen g => g -> Int -> [[Char]] -> [[Char]]
eraseGivens g0 n0 s0 
  | isNothing s1 = error "Could not erase sufficient holes"
  | otherwise = fromJust s1
  where s1 = go g0 n0 occupied s0
        dim = length s0
        occupied = [ (ri, ci) | ri <- [0 .. dim - 1]
                              , ci <- [0 .. dim - 1] ]
        go :: RandomGen g => g -> Int -> [(Int, Int)] -> [[Char]] -> Maybe [[Char]]
        go _ 0 _ s = Just s
        go g n is s 
          | null paths = Nothing
          | otherwise = Just $ head paths
          where
            ixs = shuffle' [0 .. length is - 1] (length is) g
            paths = mapMaybe (\ix -> let is' = take ix is ++ drop (ix + 1) is
                                         (r, c) = is !! ix
                                         s' = modCell (const '0') r c s
                                     in if isLeft (sudoku s')
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
                   (x, g') = randomElem g range
               in (x, g')

intsToChars :: [[Int]] -> [[Char]]
intsToChars xs = map (map intToDigit) xs
