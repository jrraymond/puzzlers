module KenKenSolver where

import System.Random
import Data.List (sortBy, tails)
import Data.Ord (comparing)


type Grid = [[Int]]
type Row = [Int]
type Constraints = Grid
type Op = [Int] -> Int
type Cage = (Int, Op, [(Int,Int)])
type KenKen = (Grid, [Cage])


combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs , ys <- combinations (n-1) xs']


sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = map snd . sortBy (comparing fst) . map (\x -> let y = f x in y `seq` (y, x))


kenken :: (Int, [([Int] -> Int, Int, [(Int,Int)])]) -> Maybe [[Int]]
kenken (d, cs0) = go cs0' empty empty where
  empty = replicate d []
  cs0' = sortOn (\(_,_,is) -> is) cs0
  combs :: Int -> [[Int]]
  combs = ((map (flip combinations [1 .. d]) [0 .. d]) !!)
  options :: ([Int] -> Int) -> Int -> Int -> [[Int]]
  options op a n = filter (\ns -> a == op ns) (combs n)
  go :: [([Int] -> Int, Int, [(Int,Int)])] -> [[Int]] -> [[Int]] -> Maybe [[Int]]
  go [] _ _ = Just []
  go (c@(op, a, ixs):cs) rcs ccs
    | null paths = Nothing
    | otherwise = Just []
    where opts = options op a (length ixs)
          paths = []

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

