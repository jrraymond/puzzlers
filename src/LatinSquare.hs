module LatinSquare where

import System.Random

genLatinSquare :: RandomGen g => g -> Int -> [[Int]]
genLatinSquare gen dim = f gen 0 0 empty empty []
  where empty = replicate dim []
        f :: RandomGen g => g -> Int -> Int 
             -> [[Int]] -> [[Int]] -> [Int] -> [[Int]]
        f g ri ci rcs ccs row
          | ri == dim = [row]
          | ci == dim = row : f g (ri + 1) 0 rcs ccs []
          | otherwise = let options = [ i | i <- [1..dim]
                                          , notElem i (rcs !! ri) 
                                            && notElem i (ccs !! ci) ]
                            (x, g') = randomElem g options
                            rcs' = addConstraint x ri rcs
                            ccs' = addConstraint x ci ccs
                        in f g' ri (ci + 1) rcs' ccs' (x : row)

addConstraint :: Int -> Int -> [[Int]] -> [[Int]]
addConstraint x i xss = take i xss ++ [x : (xss !! i)] ++ drop (i + 1) xss

randomElem :: RandomGen g => g -> [Int] -> (Int, g)
randomElem g xs = let (ix, g') = randomR (0, length xs - 1) g
                  in (xs !! ix, g')
