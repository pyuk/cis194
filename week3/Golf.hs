module Golf where

import qualified Data.List as L
--exercise 1

nList :: (Int, [b]) -> [b]
nList (a, b)
    | a <= length b = drop (a - 1) (take a b) ++ nList (a, drop a b)
    | otherwise     = [] 

skips :: [a] -> [[a]]
skips xs = map nList $ zip [1..] $ replicate (length xs) xs

--exercise 2

findMaxima :: [Integer] -> Bool
findMaxima (x:y:z:_)
    | (y > x && y > z) = True
    | otherwise        = False
findMaxima _ = False

threes :: [Integer] -> [[Integer]]
threes [] = []
threes xs = take 3 xs : threes (drop 1 xs)

takeMiddle :: [Integer] -> Integer
takeMiddle (_:y:_) = y

localMaxima :: [Integer] -> [Integer]
localMaxima xs = map takeMiddle . filter findMaxima . threes $ xs

{-findMaxima' :: [Integer] -> Integer
findMaxima' (x:y:z:xs)
    | (y > x && y > z) = y
    | otherwise        = 
localMaxima' :: [Integer] -> [Integer]
localMaxima' xs = findMaxima' (take 3 xs) : localMaxima' $ drop 1 xs-}

--exercise 3

sortAndMatch :: [Integer] -> [(Int, Integer)]
sortAndMatch [] = []
sortAndMatch xs = let l@(x:_) = L.sort xs
                      vim = takeWhile (\a -> x == a) l
--                  in  vim : sortAndMatch (filter (> x) l)
                  in  (length vim, (\(z:_) -> z) vim) : sortAndMatch (filter (> x) l)

{-pairUp :: [[Integer]] -> [(Int, Integer)]
pairUp [] = []
pairUp (xs:xss) = let x = length xs
                      y = (\(z:_) -> z) xs
                  in  (x, y) : pairUp xss -}

subtractOne :: [(Int, Integer)] -> [(Int, Integer)]
subtractOne l@((x,y):_) = let revSort = reverse . L.sort $ l
                              findHi = takeWhile (\(a,b) -> x == a) revSort
                          in  if x == 0
                                 then []
                                 else map (\(x,y) -> (x-1,y)) findHi ++ drop (length findHi) revSort

showStars :: [(Int, Integer)] -> [Integer] -> String
showStars [] [] = []
showStars [] (_:as) = " " ++ showStars [] as 
showStars pairs (a:as)
    | x == 0 = []
    | y /= a = " " ++ showStars l as
    | otherwise = "*" ++ showStars xs as
    where l@((x,y):xs) = L.sort pairs

histogram :: [Integer] -> String
histogram [] = []
histogram xs = let l@(x:revSort) = reverse . L.sort . sortAndMatch $ xs
                   group ls@((x,_):_) = takeWhile (\(a,_) -> x == a) ls
                   newGroup xs = subtractOne xs
                   histogram' [] = []
                   histogram' ns@((x,_):_) 
                       | x == 0    = []
                       | otherwise = showStars (group ns) [0..9] ++ "\n" ++ histogram' (newGroup ns)
               in  histogram' l ++ "==========\n0123456789" 
