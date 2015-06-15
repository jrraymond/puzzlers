import System.Random

type Grid = [[Int]]
type Row = [Int]
type Constraints = Grid
type Op = [Int] -> Int
type Cage = (Int, Op, [(Int,Int)])
type KenKen = (Grid, [Cage])

genLatinSquare :: RandomGen g => g -> Int -> Grid
genLatinSquare gen dim = f gen 0 0 empty empty []
  where empty = replicate dim []
        f :: RandomGen g => g -> Int -> Int 
             -> Constraints -> Constraints -> Row -> Grid
        f g ri ci rcs ccs row
          | ri == dim = [row]
          | ci == dim = row : f g (ri + 1) 0 rcs ccs []
          | otherwise = let options = [ i | i <- [1..dim]
                                          , notElem i (rcs !! ri) 
                                            && notElem i (ccs !! ci) ]
                            (ix, g') = randomR (0, length options - 1) g
                            x = options !! ix
                            rcs' = addConstraint x ri rcs
                            ccs' = addConstraint x ci ccs
                        in f g' ri (ci + 1) rcs' ccs' (x : row)

addConstraint :: Int -> Int -> [[Int]] -> [[Int]]
addConstraint x i xss = take i xss ++ [x : (xss !! i)] ++ drop (i + 1) xss


genCages :: RandomGen g => g -> [[Int]] -> [Cage]
genCages = undefined

--makeKenKen :: RandomGen g => g -> KenKen
--makeKenKen seed = (board, groups) 
--  where board = genLatinSquare seed
--        groups = genCages seed board

randomRList :: (Random a, RandomGen g) => (a, a) -> g -> Int -> ([a], g)
randomRList (lo, hi) g = f ([], g)
  where f (xs, g0) 0 = (xs, g0)
        f (xs, g0) i = let (x, g1) = randomR (lo, hi) g0
                       in f (x:xs, g1) (i - 1)

