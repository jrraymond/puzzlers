module KenKenSolver where

import System.Random

type Grid = [[Int]]
type Row = [Int]
type Constraints = Grid
type Op = [Int] -> Int
type Cage = (Int, Op, [(Int,Int)])
type KenKen = (Grid, [Cage])



genCages :: RandomGen g => g -> [[Int]] -> [Cage]
genCages = undefined

--makeKenKen :: RandomGen g => g -> KenKen
--makeKenKen seed = (board, groups) 
--  where board = genLatinSquare seed
--        groups = genCages seed board

randomRList :: (Random a, RandomGen g) => (a, a) -> g -> Int -> ([a], g)
randomRList (lo, hi) g = f ([], g)
  where f (xs, g0) 0 = (xs, g0)
        f (xs, g0) i = let (x, g1) = randomR (lo, hi) g0
                       in f (x:xs, g1) (i - 1)

