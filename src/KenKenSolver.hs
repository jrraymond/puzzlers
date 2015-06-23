module KenKenSolver where

import Data.List (tails, elemIndex, permutations, nub)
import Data.Either
import Data.Maybe (fromJust, isJust)
import LatinSquare
import KenKenGenerator




kenken :: (Int, [(Op, Int, [(Int,Int)])]) -> Either Bool [[Int]]
kenken (dim, cs0) = go cs0'' empty empty zeroes where
  empty = replicate dim []
  zeroes = replicate dim $ replicate dim 0
  cs0' = sortOn (\(_,_,is) -> length is) cs0
  cs0'' = map (\(op, s, is) -> let (rixs, cixs) = unzip is
                               in (op, s, rixs, cixs)) cs0'

  combsMemo :: Int -> [[Int]]
  combsMemo = (map combs [0 .. dim] !!)

  combs :: Int -> [[Int]]
  combs n = let f | n < 3 = combinations | otherwise = combsWithRep
            in concatMap permutations $ f n [1 .. dim]

  go :: [(Op, Int, [Int], [Int])] -> [[Int]] -> [[Int]] 
        -> [[Int]] -> Either Bool [[Int]]
  go [] _ _ sq = Right sq
  go ((op, a, rixs, cixs):cs) rcs ccs sq
    | or ls || length rs > 1 = Left True
    | null rs = Left False
    | otherwise = Right $ head rs
    where 
          opts = nub $ concatMap permutations $
                                 filter (\ns -> a == eval op ns) 
                                        (combsMemo (length rixs))
          paths = map try opts
          (ls,rs) = partitionEithers paths
          try :: [Int] -> Either Bool [[Int]]
          try [] = Left False
          try xs 
            | not (ok xs rixs cixs rcs ccs) || dups xs rixs cixs = Left False
            | otherwise = go cs rcs' ccs' sq'
            where 
              rcs' = addConstraints xs rixs rcs
              ccs' = addConstraints xs cixs ccs
              sq' = modCells xs rixs cixs sq


--returns true if elements share same index
dups :: [Int] -> [Int] -> [Int] -> Bool
dups [] _ _ = False
dups _ [] _ = False
dups _ _ [] = False
dups (x:xs) (r:rixs) (c:cixs)
  | isJust ixM && (rixs !! ix == r || cixs !! ix == c) = True
  | otherwise = dups xs rixs cixs
  where ixM = elemIndex x xs
        ix = fromJust ixM


addConstraints :: [Int] -> [Int] -> [[Int]] -> [[Int]]
addConstraints [] _ xss = xss
addConstraints _ [] xss = xss
addConstraints (x:xs) (ix:ixs) xss = let xss' = modRow (x:) ix xss
                                     in addConstraints xs ixs xss'


modCells :: [Int] -> [Int] -> [Int] -> [[Int]] -> [[Int]]
modCells [] _ _ sq = sq
modCells _ [] _ sq = sq
modCells _ _ [] sq = sq
modCells (x:xs) (r:rixs) (c:cixs) sq = let sq' = modCell (const x) r c sq
                                       in modCells xs rixs cixs sq'


ok :: [Int] -> [Int] -> [Int] -> [[Int]] -> [[Int]] -> Bool
ok xs rixs cixs rcs ccs = all (== True) $ 
                               map (\(x,r,c) -> notElem x (rcs !! r)
                                               && notElem x (ccs !! c))
                                   (zip3 xs rixs cixs)

combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs , ys <- combinations (n-1) xs']


combsWithRep :: Int -> [a] -> [[a]]
combsWithRep k xs = combsBySize xs !! k
 where combsBySize = foldr f ([[]] : repeat [])
       f :: a -> [[[a]]] -> [[[a]]]
       f x = scanl1 (\z n -> map (x:) z ++ n)

