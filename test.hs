import Data.List
type Grid = Matrix Value 
type Matrix a = [Row a]
type Row a = [a]
type Value = Char 
--equivalent to type Grid = [[Char]]
--input of sudou
minimal               :: Grid
minimal               =  [".98......",
                           "....7....",
                           "....15...",
                           "1........",
                           "...2....9",
                           "...9.6.82",
                           ".......3.",
                           "5.1......",
                           "...4...2."]

--validity check of Sudoku(no repetition of numbers in rows or columns)
rows :: Matrix a -> [Row a]
rows  = id--identity function

cols :: Matrix a ->[Row a]
cols = transpose 
boxsize =3
boxs :: Matrix a -> [Row a]
boxs =  unpack . map cols . pack
        where
            pack   = split . map split
            split  = chop boxsize
            unpack = map concat . concat  -- pulls a box as a row

chop :: Int->[a]->[[a]]
chop n [] = []
chop n xs = take n xs: chop n(drop n xs)
-- check validity of sudoku
valid :: Grid -> Bool
valid g = all nodups (rows g) && all nodups (cols g) && all nodups (boxs g) 

nodups :: Eq a => [a] -> Bool
nodups [] = True
nodups (x:xs) = not(elem x xs) && nodups xs

-- basic solver 
-- solve :: Grid -> [Grid]-- gives all solutions to the sudoku
-- solve = filter valid . collapse . choices 

type Choices = [Value]
choices :: Grid -> Matrix Choices 
choices g = map (map choice) g 
            where 
                choice v = if v == '.' then
                    ['1'..'9']
                else 
                    [v]

collapse :: Matrix[a] -> [Matrix a]
collapse m = cp(map cp m) -- inner part collapses row// -- outer cp collapses column
-- cartesian product:
cp :: [[a]] -> [[a]]
cp [] = [[]] -- [] would make this function always return []
cp (xs:xss) = [y:ys| y<-xs, ys<- cp xss]

-- but this is explosive in complexity so does not work in practice 
-- how to prune the search base?=> main rules of the game
prune :: Matrix Choices -> Matrix Choices
prune = pruneBy boxs . pruneBy cols . pruneBy rows
        where pruneBy f = f . map reduce . f
single :: [a]->Bool
single [_] = True
single _ = False
reduce :: Row Choices -> Row Choices 
reduce xss = [xs `minus` singles| xs<- xss]
            where singles = concat(filter single xss)
minus :: Choices->Choices->Choices 
xs `minus` ys = if single xs then xs else xs \\ ys

-- solve2 = filter valid. collapse . prune. choices
-- this is still not feasible, need more thinking
-- soln = repeated pruning -> keep pruning until we reach a fixed point 

-- solve3 = filter valid. collapse .fix prune. choices
-- fix :: Eq a =>(a->a)->a->a
-- fix f x = if x == x' then x else fix f x'
--           where x' = f x
-- this solves the easy grid but again blows up 

void :: Matrix Choices -> Bool
void m = any(any null) m  -- does any row have an empty cell?

safe:: Matrix Choices -> Bool
safe m = all consistent (rows m) && all consistent (cols m) && all consistent (boxs m) 

consistent :: Row Choices -> Bool
consistent  = nodups . concat . filter single

blocked :: Matrix Choices -> Bool
blocked m = void m || not(safe m)

solve4 = search . prune . choices 

search :: Matrix Choices -> [Grid]
search m| blocked m = []
        | all(all single)m = collapse m
        | otherwise = [g| m' <- expand m, g <- search(prune m')]

expand :: Matrix Choices -> [Matrix Choices]
expand m =  [rows1 ++ [row1 ++ [c]: row2]++rows2|c<-cs]
            where
                (rows1, row:rows2) = break(any(not.single)) m
                (row1 , cs:row2)  = break(not. single) row

main :: IO()
main =putStrLn(unlines (head (solve4 minimal)))

