module Main (main) where

import Criterion.Main
import System.Random
import System.Console.GetOpt
import System.Environment (getArgs, withArgs)
import Data.List (intercalate)
import Data.Either (partitionEithers)
import Text.Read (readMaybe)

import SudokuSolver
import SudokuGenerator
import KenKenSolver
import KenKenGenerator
import LatinSquare


main :: IO ()
main = do (o,n) <- parseOpts =<< getArgs
          withArgs n $ defaultMain [getBench o]


data Flag = Sudoku | KenKen | Generate | Solve | Diff Int deriving (Read, Show, Eq)
          

sudokuGs :: [Benchmark]
sudokuGs = [ bench "sudoku gen easiest"  $ nf (genSudoku (mkStdGen 0)) Easiest
           , bench "sudoku gen easy"     $ nf (genSudoku (mkStdGen 0)) Easy
           , bench "sudoku gen moderate" $ nf (genSudoku (mkStdGen 0)) Moderate
           , bench "sudoku gen hard"     $ nf (genSudoku (mkStdGen 0)) Hard
           , bench "sudoku gen evil"     $ nf (genSudoku (mkStdGen 0)) Evil
           ]
sudokuSs :: [Benchmark]
sudokuSs = [ bench "sudoku solve easiest"  $ nf sudoku (sudokus 0 0)
           , bench "sudoku solve easy"     $ nf sudoku (sudokus 1 0)
           , bench "sudoku solve moderate" $ nf sudoku (sudokus 2 0)
           , bench "sudoku solve hard"     $ nf sudoku (sudokus 3 0)
           , bench "sudoku solve evil"     $ nf sudoku (sudokus 4 0)
           ]
kenkenGs :: [Benchmark]
kenkenGs = [ bench "kenken gen 4x4" $ nf (genKenKen (mkStdGen 0)) 4
           , bench "kenken gen 6x6" $ nf (genKenKen (mkStdGen 1)) 6
           ]
kenkenSs :: [Benchmark]
kenkenSs = [ bench "kenkeng solve 4x4" $ nf kenken (mapFst length $ kenkens 4 0)
           , bench "kenkeng solve 6x6" $ nf kenken (mapFst length $ kenkens 6 0)
           ]

getBench :: [Flag] -> Benchmark
getBench [] = bgroup "running all" $ sudokuGs ++ sudokuSs ++ kenkenGs ++ kenkenSs
getBench fs 
  | Sudoku `elem` fs && Generate `elem` fs = sudokuGs !! i
  | Sudoku `elem` fs = sudokuSs !! i
  | KenKen `elem` fs && Generate `elem` fs = sudokuGs !! (max i (length kenkenGs - 1))
  | KenKen `elem` fs = kenkenSs !! (max i (length kenkenSs - 1))
  | otherwise = error "incorrect flags"
  where i = getDiff fs



options :: [OptDescr (Either String Flag)]
options = [ Option "p" ["puzzle"] (ReqArg puzzle "Sudoku/KenKen") "sudoku or kenken"
          , Option "a" ["action"] (ReqArg action "Generate/Solve") "generate or solve"
          , Option "d" ["difficulty"] (ReqArg diff "[0..4]") "difficult levels 0-4"
          ]

parseOpts :: [String] -> IO ([Flag], [String])
parseOpts argv = 
    case getOpt Permute options argv of
         (o,n,[]  ) -> let (parseErrs, fs) = partitionEithers o
                       in if null parseErrs 
                            then return (fs,n)
                            else ioError . userError $ usageInfo header options ++ intercalate "\n" parseErrs
         (_,_,errs) -> ioError . userError $ usageInfo header options ++ concat errs
    where header = "Usage: "


getDiff :: [Flag] -> Int
getDiff [] = error "no difficulty in flags"
getDiff (Diff x:_) = x
getDiff fs = getDiff (tail fs)

puzzle :: String -> Either String Flag
puzzle s = case readMaybe s of
             Just f -> Right f
             Nothing -> Left "USAGE: -p [--puzzle] <Sudoku/KenKen>"

action :: String -> Either String Flag
action s = case readMaybe s of
             Just f  -> Right f
             Nothing -> Left "USAGE: -a [--action] <Generate/Solve>"

diff :: String -> Either String Flag
diff s = case readMaybe s of
           Just x -> Right (Diff x)
           Nothing -> Left "USAGE: -d [--Diff] Diff <0,1,2,3,4>"

sudokus :: Int -> Int -> [[Int]]
sudokus d i = snd (genSudoku (mkStdGen i) (difficulties !! d))

kenkens :: Int -> Int -> ([[Int]], [(Op, Int, [(Int,Int)])])
kenkens d i = genKenKen (mkStdGen i) d
