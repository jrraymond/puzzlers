module KenKenGenerator where

import LatinSquare
import System.Random
import Data.List (foldl1', mapAccumL)

data Op = Plus | Minus | Multiply | Divide deriving (Show, Eq, Read)

genKenKen :: RandomGen g => g -> Difficulty -> Int -> ([[Int]], [(Op, Int, [(Int,Int)])])
genKenKen g diff dim = let sq = genLatinSquare g dim latinSqRules
                           cages = genCages g diff dim sq 
                           ops = genOps g diff sq cages
                       in (sq, ops)

genCages :: RandomGen g => g -> Difficulty -> Int -> [[Int]] -> [[(Int,Int)]]
genCages g0 diff dim sq0 = go g0 (fillIndices sq0) []
  where cage_sizes = getCageDist diff
        fillIndices :: [[Int]] -> [(Int,Int,Int)]
        fillIndices xss = snd $ mapAccumL (\i x -> (i + 1, (x,div i dim,mod i dim)))
                                          0
                                          (concat xss)
        go :: RandomGen g => g -> [(Int,Int,Int)] -> [(Int,Int)] -> [[(Int,Int)]]
        go g xs ys 
          | length xs == length ys = []
          | otherwise = let (n0, g1) = randomElem g cage_sizes
                            n = let xsl = length xs
                                in if n0 < xsl then n0 else xsl
                            xs_left = filter (`notElem` ys) (map (\(_,ri,ci) -> (ri,ci)) xs)
                            (z0@(r,c), g2) = randomElem g1 xs_left
                            zs = getCage g2 dim (n - 1) (r,c) ((r,c):ys)
                            zs' = z0 : zs
                            ys' = zs' ++ ys
                            --(f, g3) = randomElem g2 operations
                        in zs' : go g2 xs ys'

getCage :: RandomGen g => g -> Int -> Int -> (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
getCage g dim n (r,c) ys
  | n < 1 || null ixs = []
  | otherwise = ix : getCage g' dim (n - 1) ix (ix : ys)
  where ixs = filter (`notElem` ys) $ getAdjIxs dim (r, c)
        (ix, g') = randomElem g ixs


getAdjIxs :: Int -> (Int,Int) -> [(Int,Int)]
getAdjIxs dim (r,c) = filter (\(ri,ci) -> ri >= 0 && ri < dim && ci >= 0 && ci < dim)
                             [(r - 1,c),(r,c - 1),(r + 1,c),(r,c + 1)]

genOps :: RandomGen g => g -> Difficulty -> [[Int]] -> [[(Int,Int)]] -> [(Op, Int, [(Int,Int)])]
genOps _ _ _ [] = []
genOps g0 diff sq (cs:css) = let (f, g1) = randomElem g0 operations
                                 s = eval f [ sq !! r !! c | (r,c) <- cs] 
                             in (f, s, cs) : genOps g1 diff sq css

getCageDist :: Difficulty -> [Int]
getCageDist d = case d of 
                  Easiest  -> [1, 2, 2, 2, 2]
                  Easy     -> [2, 2, 2, 2, 3]
                  Moderate -> [2, 2, 2, 3, 3]
                  Hard     -> [2, 2, 3, 3, 4]
                  Evil     -> [2, 2, 3, 4, 4]

eval :: Op -> [Int] -> Int
eval op = case op of
            Plus     -> foldl1' (+)
            Minus    -> foldl1' (-)
            Multiply -> foldl1' (*)
            Divide   -> foldl1' div


operations :: [Op]
operations = [Plus, Minus, Multiply, Divide]


