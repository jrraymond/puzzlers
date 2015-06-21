module SudokuSolver where

import Data.Maybe
import LatinSquare

type Sudoku = [[Int]]
type Board = [[(Int,Int,Int)]]
type Row = [(Int,Int,Int)]
type Col = Row
type Box = Col

sudoku :: [[Int]] -> Maybe [[Int]]
sudoku s0 = go s0 rcs0 ccs0 bcs0 0 0 where
  rcs0 = map (filter (/= 0)) s0
  ccs0 = map (filter (/= 0)) (columns s0)
  bcs0 = map (filter (/= 0)) (boxes s0)
  go :: [[Int]] -> [[Int]] -> [[Int]] -> [[Int]]
         -> Int -> Int -> Maybe [[Int]]
  go s _ _ _ 9 _ = Just s
  go s rcs ccs bcs r 9 = go s rcs ccs bcs (r + 1) 0
  go s rcs ccs bcs r c
    | cell /= 0 = go s rcs ccs bcs r (c + 1)
    | null paths = Nothing
    | otherwise = Just $ head paths
    where cell = s0 !! r !! c
          bIx = getBoxI r c
          options = [ x | x <- [1 .. 9]
                        , notElem x (rcs !! r) && 
                          notElem x (ccs !! c) &&
                          notElem x (bcs !! bIx) ]
          paths = mapMaybe (\x -> let rcs' = addConstraint x r rcs
                                      ccs' = addConstraint x c ccs
                                      bcs' = addConstraint x bIx bcs
                                      s' = modCell (const x) r c s
                                  in go s' rcs' ccs' bcs' r (c + 1))
                            options

columns :: [[Int]] -> [[Int]]
columns s0 = go 0 0 (replicate 9 []) where
  go 9 _ ccs = ccs
  go r 9 ccs = go (r + 1) 0 ccs
  go r c ccs = go r (c + 1) (addConstraint (s0 !! r !! c) c ccs)

boxes :: [[Int]] -> [[Int]]
boxes s0 = go 0 0 (replicate 9 []) where
  go 9 _ bcs = bcs
  go r 9 bcs = go (r + 1) 0 bcs
  go r c bcs = let v = s0 !! r !! c
                   bcs' = addConstraint v (getBoxI r  c) bcs
               in go r (c + 1) bcs'

