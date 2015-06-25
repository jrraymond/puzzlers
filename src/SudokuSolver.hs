module SudokuSolver where

import Data.Either
import LatinSquare


sudoku :: [[Char]] -> Either Bool [[Char]]
sudoku s0 = go s0 rcs0 ccs0 bcs0 0 0 where
  rcs0 = map (filter (/= '0')) s0
  ccs0 = map (filter (/= '0')) (columns s0)
  bcs0 = map (filter (/= '0')) (boxes s0)
  go :: [[Char]] -> [[Char]] -> [[Char]] -> [[Char]]
         -> Int -> Int -> Either Bool [[Char]]
  go s _ _ _ 9 _ = Right s
  go s rcs ccs bcs r 9 = go s rcs ccs bcs (r + 1) 0
  go s rcs ccs bcs r c
    | cell /= '0' = go s rcs ccs bcs r (c + 1)
    | or ls || length rs > 1 = Left True
    | null rs = Left False
    | otherwise = Right $ head rs
    where cell = s0 !! r !! c
          bIx = getBoxI r c
          (ls, rs) = partitionEithers paths
          options = [ x | x <- ['1' .. '9']
                        , notElem x (rcs !! r) && 
                          notElem x (ccs !! c) &&
                          notElem x (bcs !! bIx) ]
          paths = map (\x -> let rcs' = addConstraint x r rcs
                                 ccs' = addConstraint x c ccs
                                 bcs' = addConstraint x bIx bcs
                                 s' = modCell (const x) r c s
                             in go s' rcs' ccs' bcs' r (c + 1))
                      options

columns :: [[Char]] -> [[Char]]
columns s0 = go 0 0 (replicate 9 []) where
  go 9 _ ccs = ccs
  go r 9 ccs = go (r + 1) 0 ccs
  go r c ccs = go r (c + 1) (addConstraint (s0 !! r !! c) c ccs)

boxes :: [[Char]] -> [[Char]]
boxes s0 = go 0 0 (replicate 9 []) where
  go 9 _ bcs = bcs
  go r 9 bcs = go (r + 1) 0 bcs
  go r c bcs = let v = s0 !! r !! c
                   bcs' = addConstraint v (getBoxI r  c) bcs
               in go r (c + 1) bcs'

