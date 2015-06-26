module KenKenGenerator where

import LatinSquare
import KenKenSolver
import System.Random
import Data.List (partition, intersect, delete)
import Data.Either (isLeft)

genKenKen :: RandomGen g => g -> Int -> ([[Int]], [(Op, Int, [(Int,Int)])])
genKenKen g0 dim = (sq, try g0 0 sq cages)
    where sq = genLatinSquare g0 dim latinSqRules
          cages = genCages g0 dim
          try :: RandomGen g => g -> Int -> [[Int]] -> [[(Int,Int)]] -> [(Op, Int, [(Int,Int)])]
          try g n sq0 cs
            | n > 99 = error "could not generate unique kenken after 10 trys"
            | isLeft (kenken (dim, ops)) = try (snd $ next g) (n + 1) sq0 cs'
            | otherwise = ops
            where ops = genOps g sq0 cs
                  cs' = genCages g dim

genOps :: RandomGen g => g -> [[Int]] -> [[(Int,Int)]] -> [(Op, Int, [(Int,Int)])]
genOps _ _ [] = []
genOps g0 sq (cs:css) = let (f, g1) = randomElem g0 operations
                            s = eval f [ sq !! r !! c | (r,c) <- cs] 
                        in (f, s, cs) : genOps g1 sq css


genCages :: RandomGen g => g -> Int -> [[(Int,Int)]]
genCages g0 dim = (reduceSingles dim . sortOn length) $ go g0 ixs0 []
  where ixs0 = [ (r,c) | r <- [0 .. dim - 1], c <- [0 .. dim - 1] ]
        sizes = [2,2,3,3]
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

-- dim -> cages -> new cages with num singles <= max singles
reduceSingles :: Int -> [[(Int,Int)]] -> [[(Int,Int)]]
reduceSingles dim xs 
  | num_singles <= max_singles = xs 
  | otherwise = reduceSingles dim cages'
  where max_singles = dim `div` 2
        num_singles = length singles
        (singles, rest) = partition (\x -> 1 == length x) xs
        single = head $ head singles
        to_join = head $ getAdjCages dim single xs
        rest' = delete to_join (tail singles ++ rest)
        cages' = rest' ++ [single : to_join]


