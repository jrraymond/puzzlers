module ParseArgs where


import System.Console.GetOpt
import Data.List (intercalate)
import Data.Either (partitionEithers)
import Text.Read (readMaybe)

data Flag = Sudoku | KenKen | Generate | Solve | Diff Int deriving (Read, Show, Eq)


options :: [OptDescr (Either String Flag)]
options = [ Option "p" ["puzzle"] (ReqArg puzzle "Sudoku/KenKen") "sudoku or kenken"
          , Option "a" ["action"] (ReqArg action "Generate/Solve") "generate or solve"
          , Option "d" ["difficulty"] (OptArg diff "[0..4]") "difficult levels 0-4"
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
getDiff [] = (-1)
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

diff :: Maybe String -> Either String Flag
diff Nothing = Right (Diff (-1))
diff (Just s) = case readMaybe s of
                   Just x -> Right (Diff x)
                   Nothing -> Left "USAGE: -d [--Diff] Diff <0,1,2,3,4>"

