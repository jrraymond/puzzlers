module KenKenGenerator where

import LatinSquare
import System.Random
import Data.List (foldl1')

genKenKen :: RandomGen g => g -> Int -> ([[Int]], [([Int] -> Int, Int, [(Int,Int)])])
genKenKen g dim = let sq = genLatinSquare g dim
                      cages = genCages (stepG g) sq 
                      ops = genOps (stepG g) sq cages
                  in (sq, ops)

genCages :: RandomGen g => g -> [[Int]] -> [[(Int,Int)]]
genCages = undefined

genOps :: RandomGen g => g -> [[Int]] -> [[(Int,Int)]] -> [([Int] -> Int, Int, [(Int,Int)])]
genOps = undefined

getCageDist :: Difficulty -> [Int]
getCageDist d = case d of 
                  Easiest  -> [1, 1, 2, 2, 2]
                  Easy     -> [1, 2, 2, 2, 3]
                  Moderate -> [1, 2, 2, 3, 3]
                  Hard     -> [1, 2, 3, 3, 4]
                  Evil     -> [1, 2, 3, 4, 4]
                 -- Easiest  -> [1, 1, 1, 2, 2, 2, 2, 2, 3, 3]
                 -- Easy     -> [1, 1, 2, 2, 2, 2, 2, 2, 3, 3]
                 -- Moderate -> [1, 1, 2, 2, 2, 2, 3, 3, 3, 4]
                 -- Hard     -> [1, 2, 2, 2, 2, 3, 3, 3, 4, 4]
                 -- Evil     -> [1, 2, 2, 2, 3, 3, 3, 4, 4, 4]

plus :: [Int] -> Int
plus = foldl1' (+)

minus :: [Int] -> Int
minus = foldl1' (-)

multiply :: [Int] -> Int
multiply = foldl1' (*)

divide :: [Int] -> Int
divide = foldl1' div
