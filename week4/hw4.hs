--exercise 1

fun1 :: [Integer] -> Integer
fun1 []     = 1
fun1 (x:xs)
    | even x    = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even 

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/=1) . iterate checkNum
    where checkNum x = if even x then x `div` 2 else 3*x + 1

--exercise 2

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: (Eq a) => [a] -> Tree a
foldTree = foldr addToTree Leaf

addToTree :: (Eq a) => a -> Tree a -> Tree a
addToTree a Leaf = Node 0 Leaf a Leaf
addToTree a tree@(Node _ x y z)
    | showHeight x <= showHeight z = Node height (addToTree a x) y z
    | otherwise                    = Node height x y (addToTree a z)
    where height = calcHeight tree

showHeight :: Tree a -> Integer
showHeight Leaf = -1
showHeight (Node a _ _ _) = a

calcHeight :: Tree a -> Integer
calcHeight Leaf = -1
calcHeight (Node _ x _ z) = 1 + calcHeight tree 
    where tree = if showHeight x >= showHeight z then x else z

--exercise 3

xor :: [Bool] -> Bool
xor = foldr counter False
    where counter a b
            | (a == False && b == False) = False 
            | (a == True && b == False) = True 
            | (a == False && b == True) = True 
            | (a == True && b == True) = False 

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> f x : y) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\x y -> y `f` x) base xs

--exercise 4

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\n -> 2*n+1) . filter (\x -> not (x `elem` (f n))) . allInts $ n
    where allInts n = [1..n]
          f n       = [i+j+2*i*j | i <- [1..n], j <- [1..n], i <= j, i+j+2*i*j <= n]

sieveSundaram' :: Integer -> [Integer]
sieveSundaram' n = map (\n -> 2*n + 1) . map joinIJ . filter (filteredIJ n) . allInts $ n
    where allInts n          = cartProd [1..n] [1..n]
          filteredIJ n (i,j) = (i <= j) && (i+j+2*i*j <= n)
          joinIJ (i,j)       = i + j + 2*i*j
