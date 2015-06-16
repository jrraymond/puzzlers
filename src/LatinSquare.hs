module LatinSquare where

import System.Random
import Data.Maybe (fromJust, mapMaybe)

genLatinSquare :: RandomGen g => g -> Int -> [[Int]]
genLatinSquare gen dim = fromJust $ go gen 0 0 empty empty zeroes
  where empty = replicate dim []
        zeroes = replicate dim $ replicate dim 0
        go :: RandomGen g => g -> Int -> Int 
             -> [[Int]] -> [[Int]] -> [[Int]] -> Maybe [[Int]]
        go g ri ci rcs ccs sq
          | ri == dim = Just sq
          | ci == dim = go g (ri + 1) 0 rcs ccs sq
          | null paths = Nothing
          | otherwise = Just s
          where options = [ i | i <- [1..dim]
                              , notElem i (rcs !! ri) 
                                && notElem i (ccs !! ci) ]
                (s, g') = randomElem g paths
                paths = mapMaybe (\x -> let rcs' = addConstraint x ri rcs
                                            ccs' = addConstraint x ci ccs
                                            sq' = modCell (const x) ri ci sq
                                        in go g' ri (ci + 1) rcs' ccs' sq')
                                  options

addConstraint :: Int -> Int -> [[Int]] -> [[Int]]
addConstraint x i xss = take i xss ++ [x : (xss !! i)] ++ drop (i + 1) xss

modCell :: (Int -> Int) -> Int -> Int -> [[Int]] -> [[Int]]
modCell f ri ci xss = let xs = xss !! ri
                          c' = f (xs !! ci)
                          xs' = take ci xs ++ c' : drop (ci + 1) xs
                      in take ri xss ++ [xs'] ++ drop (ri + 1) xss

randomElem :: RandomGen g => g -> [a] -> (a, g)
randomElem g xs = let (ix, g') = randomR (0, length xs - 1) g
                  in (xs !! ix, g')
