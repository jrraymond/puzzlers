module LatinSquare where

import System.Random
import Data.Maybe (fromJust, mapMaybe)
import Data.List (intercalate, sortBy)
import Data.Ord (comparing)

data Difficulty = Easiest | Easy | Moderate | Hard | Evil deriving (Show, Read, Eq)

genLatinSquare :: RandomGen g => g -> Int -> (Int -> Int -> [[Int]] -> Int -> Bool) -> [[Int]]
genLatinSquare gen dim rules = fromJust $ go gen 0 0 zeroes
  where zeroes = replicate dim $ replicate dim 0
        go :: RandomGen g => g -> Int -> Int -> [[Int]] -> Maybe [[Int]]
        go g ri ci sq
          | ri == dim = Just sq
          | ci == dim = go g (ri + 1) 0 sq
          | null paths = Nothing
          | otherwise = Just s
          where ok :: Int -> Bool
                ok = rules ri ci sq
                options = filter ok [1 .. dim]
                (s, g') = randomElem g paths
                paths = mapMaybe (\x -> let sq' = modCell (const x) ri ci sq
                                        in go g' ri (ci + 1) sq')
                                  options


latinSqRules :: Int -> Int -> [[Int]] -> Int -> Bool
latinSqRules r c sq x = notElem x (sq !! r) && notElem x (map (!! c) sq)

sudokuRules  :: Int -> Int -> [[Int]] -> Int -> Bool
sudokuRules r c sq x = let bi = getBoxI r c
                           d = length sq
                           box = [ sq !! ri !! ci | ri <- [0 .. d - 1]
                                                  , ci <- [0 .. d - 1]
                                                  , bi == getBoxI ri ci ]
                       in notElem x box && latinSqRules r c sq x


addConstraint :: Int -> Int -> [[Int]] -> [[Int]]
addConstraint x i xss = take i xss ++ [x : (xss !! i)] ++ drop (i + 1) xss


modRow :: ([Int] -> [Int]) -> Int -> [[Int]] -> [[Int]]
modRow f i xss = take i xss ++ [f (xss !! i)] ++ drop (i + 1) xss


modCell :: (Int -> Int) -> Int -> Int -> [[Int]] -> [[Int]]
modCell f ri ci xss = let xs = xss !! ri
                          c' = f (xs !! ci)
                          xs' = take ci xs ++ c' : drop (ci + 1) xs
                      in take ri xss ++ [xs'] ++ drop (ri + 1) xss

randomElem :: RandomGen g => g -> [a] -> (a, g)
randomElem g xs = let (ix, g') = randomR (0, length xs - 1) g
                  in (xs !! ix, g')

getBoxI :: Int -> Int -> Int
getBoxI r c = 3 * (r `div` 3) + (c `div` 3)

showGrid :: Show a => [[a]] -> String
showGrid = intercalate "\n" . map show

sub :: Eq a => [a] -> [a] -> [a]
sub as bs = [ a | a <- as, a `notElem` bs ]

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = map snd . sortBy (comparing fst) . map (\x -> let y = f x in y `seq` (y, x))
