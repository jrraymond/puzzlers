module KenKenGenerator where

import LatinSquare
import System.Random
import Data.List (foldl1', partition, intersect, delete)
import Debug.Trace

data Op = Plus | Minus | Multiply | Divide deriving (Show, Eq, Read)

genKenKen :: RandomGen g => g -> Difficulty -> Int -> ([[Int]], [(Op, Int, [(Int,Int)])])
genKenKen g diff dim = let sq = genLatinSquare g dim latinSqRules
                           cages = genCages g dim
                           ops = genOps g diff sq cages
                       in (sq, ops)

genCages :: RandomGen g => g -> Int -> [[(Int,Int)]]
genCages g0 dim = (reduceSingles . sortOn length) $ go g0 ixs0 []
  where ixs0 = [ (r,c) | r <- [0 .. dim - 1], c <- [0 .. dim - 1] ]
        sizes = [2,2,3,3]
        max_singles = dim `div` 2
        go :: RandomGen g => g -> [(Int,Int)] -> [(Int,Int)] -> [[(Int,Int)]]
        go g xs ys
          | null xs = []
          | otherwise = zs : go (snd $ next g2) xs' ys'
          where (n0, g1) = randomElem g sizes
                xsl = length xs
                n | n0 < xsl = n0 | otherwise = xsl
                (rc, g2) = randomElem g1 xs
                zs = rc : getCage g2 dim (n - 1) rc (rc:ys)
                ys' = zs ++ ys
                xs' = sub xs zs
        reduceSingles :: [[(Int,Int)]] -> [[(Int,Int)]]
        reduceSingles xs 
          | num_singles <= max_singles = xs 
          | otherwise = reduceSingles cages'
          where num_singles = length singles
                (singles, rest) = partition (\x -> 1 == length x) xs
                single = head $ head singles
                to_join = head $ getAdjCages dim single xs
                rest' = delete to_join (tail singles ++ rest)
                cages' = rest' ++ [single : to_join]
                

                


getCage :: RandomGen g => g -> Int -> Int -> (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
getCage g dim n (r,c) ys
  | n < 1 || null ixs = []
  | otherwise = ix : getCage g' dim (n - 1) ix (ix : ys)
  where ixs = filter (`notElem` ys) $ getAdjIxs dim (r, c)
        (ix, g') = randomElem g ixs


getAdjIxs :: Int -> (Int,Int) -> [(Int,Int)]
getAdjIxs dim (r,c) = filter (\(ri,ci) -> ri >= 0 && ri < dim && ci >= 0 && ci < dim)
                             [(r - 1,c),(r,c - 1),(r + 1,c),(r,c + 1)]

getAdjCages :: Int -> (Int,Int) -> [[(Int,Int)]] -> [[(Int,Int)]]
getAdjCages dim rc xs = let is = getAdjIxs dim rc
                        in [ x | x <- xs , not $ null (is `intersect` x) ]

genOps :: RandomGen g => g -> Difficulty -> [[Int]] -> [[(Int,Int)]] -> [(Op, Int, [(Int,Int)])]
genOps _ _ _ [] = []
genOps g0 diff sq (cs:css) = let (f, g1) = randomElem g0 operations
                                 s = eval f [ sq !! r !! c | (r,c) <- cs] 
                             in (f, s, cs) : genOps g1 diff sq css


eval :: Op -> [Int] -> Int
eval op = case op of
            Plus     -> foldl1' (+)
            Minus    -> foldl1' (-)
            Multiply -> foldl1' (*)
            Divide   -> foldl1' div


operations :: [Op]
operations = [Plus, Minus, Multiply, Divide]
