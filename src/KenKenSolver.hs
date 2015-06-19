module KenKenSolver where

import Data.List (sortBy, tails, elemIndex, permutations, nub)
import Data.Ord (comparing)
import Data.Maybe (mapMaybe, fromJust, isJust)
import LatinSquare


type Grid = [[Int]]
type Row = [Int]
type Constraints = Grid
type Op = [Int] -> Int
type Cage = (Int, Op, [(Int,Int)])
type KenKen = (Grid, [Cage])


kenken :: (Int, [([Int] -> Int, Int, [(Int,Int)])]) -> Maybe [[Int]]
kenken (dim, cs0) = go cs0'' empty empty zeroes where
  empty = replicate dim []
  zeroes = replicate dim $ replicate dim 0
  cs0' = sortOn (\(_,_,is) -> length is) cs0
  cs0'' = map (\(op, s, is) -> let (rixs, cixs) = unzip is
                               in (op, s, rixs, cixs)) cs0'

  combsMemo :: Int -> [[Int]]
  combsMemo = ((map combs [0 .. dim]) !!)

  combs :: Int -> [[Int]]
  combs n = let f | n < 3 = combinations | otherwise = combsWithRep
            in concatMap permutations $ f n [1 .. dim]

  go :: [([Int] -> Int, Int, [Int], [Int])] -> [[Int]] -> [[Int]] 
        -> [[Int]] -> Maybe [[Int]]
  go [] _ _ sq = Just sq
  go ((op, a, rixs, cixs):cs) rcs ccs sq
    | length paths /= 1 = Nothing
    | otherwise = Just $ head paths
    where opts = nub $ concatMap permutations $
                                 filter (\ns -> a == op ns) 
                                        (combsMemo (length rixs))
          paths = mapMaybe try opts
          try :: [Int] -> Maybe [[Int]]
          try [] = Nothing
          try xs 
            | not (ok xs rixs cixs rcs ccs) = Nothing
            | dups xs rixs cixs = Nothing
            | otherwise = go cs rcs' ccs' sq'
            where 
              rcs' = addConstraints xs rixs rcs
              ccs' = addConstraints xs cixs ccs
              sq' = modCells xs rixs cixs sq


--returns true if elements share same index
dups :: [Int] -> [Int] -> [Int] -> Bool
dups [] rixs cixs = False
dups (x:xs) (r:rixs) (c:cixs)
  | isJust ixM && (rixs !! ix == r || cixs !! ix == c) = True
  | otherwise = dups xs rixs cixs
  where ixM = elemIndex x xs
        ix = fromJust ixM


addConstraints :: [Int] -> [Int] -> [[Int]] -> [[Int]]
addConstraints [] ixs xss = xss
addConstraints (x:xs) (ix:ixs) xss = let xss' = modRow (x:) ix xss
                                     in addConstraints xs ixs xss'


modCells :: [Int] -> [Int] -> [Int] -> [[Int]] -> [[Int]]
modCells [] rixs cixs sq = sq
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


sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = map snd . sortBy (comparing fst) . map (\x -> let y = f x in y `seq` (y, x))

combsWithRep :: Int -> [a] -> [[a]]
combsWithRep k xs = combsBySize xs !! k
 where combsBySize = foldr f ([[]] : repeat [])
       f x nex = scanl1 (\z n -> map (x:) z ++ n) nex

