module SudokuSolver where
import Data.Maybe

type Sudoku = [[Int]]
data Difficulty = Easiest | Easy | Moderate | Hard | Evil
type Board = [[(Int,Int,Int)]]
type Row = [(Int,Int,Int)]
type Col = Row
type Box = Col

sudoku :: [[Int]] -> [[Int]]
sudoku board 
  | isNothing solution = board
  | otherwise = removeIndexes $ fromJust solution
  where board' = populateIndexes board 
        solution = sudokuH board' board' 0 0


sudokuH :: Board -> Board -> Int -> Int -> Maybe Board
sudokuH _ ys 9 _ = Just ys
sudokuH xs ys r 9 = sudokuH xs ys (r + 1) 0
sudokuH xs ys r c 
  | v /= 0 = sudokuH xs ys r (c + 1)
  | null paths = Nothing
  | otherwise = Just $ head paths
  where (v,_,_) = xs !! r !! c
        ysR = ys !! r
        ysC = getCol ys c
        ysBox = getBox ys r c
        choices = validInts ysR ysC ysBox
        paths = mapMaybe (\i -> sudokuH xs (update ys r c i) r (c + 1)) choices

update :: Board -> Int -> Int -> Int -> Board
update b r c v = let row = b !! r
                     row' = take c row ++ [(v,r,c)] ++ drop (c + 1) row
                 in take r b ++ [row'] ++ drop (r + 1) b

validInts :: Row -> Col -> Box ->[Int]
validInts row col box = filter (\c -> ok row c && ok col c && ok box c) [1..9]

ok :: Row -> Int -> Bool
ok xs i = not $ any (\(v,_,_) -> v == i) xs

getCol :: Board -> Int -> Col
getCol b i = map (!! i) b

getBox :: Board -> Int -> Int -> Box
getBox b r c = let bI = getBoxI r c
               in filter (\(_,ri,ci) -> getBoxI ri ci == bI) $ concat b

getBoxI :: Int -> Int -> Int
getBoxI r c = 3 * (r `div` 3) + (c `div` 3)


populateIndexes :: [[Int]] -> Board
populateIndexes = zipWith (\i c -> zipWith 
                                    (\ci (ri,cv) ->  (cv, ri, ci)) [0..] 
                                    (map ((,) i) c)) 
                          [0..]

removeIndexes :: Board -> [[Int]]
removeIndexes = map (map (\(cv,_,_) -> cv))

boardToStr :: Board -> String
boardToStr = f 0 
  where f :: Int -> Board -> String
        f _ [] = replicate 19 '_'
        f i (r:rs)
          | i `mod` 3 == 0 = replicate 19 '_' ++ "\n" ++ rowToStr r ++ "\n" ++ f (i + 1) rs 
          | otherwise = rowToStr r ++ "\n" ++ f (i + 1) rs


rowToStr :: Row -> String
rowToStr [] = ""
rowToStr ((v,_,i):cs) = pre ++ show v ++ post ++ rowToStr cs
  where pre | i `mod` 3 == 0 = "|" | otherwise = " "
        post | i == 8 = "|" | otherwise = ""
            
