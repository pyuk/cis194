module Party where

import Employee
import Data.Tree

--exercise 1

glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp { empFun = n }) (GL xs x) = GL (e:xs) (x + n)

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL xs x) (GL ys y) = GL (xs ++ ys) (x + y)
    mconcat = foldr mappend mempty

moreFun :: GuestList -> GuestList -> GuestList
moreFun e@(GL _ x) e2@(GL _ y)
    | x > y = e
    | otherwise = e2

--exercise 2

treeFold :: (a -> [b] -> b) -> Tree a -> b
--treeFold f (Node { rootLabel = a, subForest = [] }) = f a []
treeFold f (Node { rootLabel = a, subForest = xs }) = 
    a `f` map (treeFold f) xs

--exercise 3

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e g = (mconcat . map findFun $ g, mconcat . map findFun . map addBoss $ g)
    where findFun (a,b) = moreFun a b
          addBoss (a,b) = (glCons e a,glCons e b)

nextLevel' :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel' e [] = (glCons e mempty, mempty)
nextLevel' e g = (boss g, noBoss g)
    where boss = glCons e . mconcat . map snd
          noBoss = mconcat . map fst

--exercise 4

maxFun :: Tree Employee -> GuestList
maxFun = findFun . treeFold nextLevel'
    where findFun (a,b) = moreFun a b

--exercise 5

formatGL :: GuestList -> String
formatGL (GL xs n) = unlines $ ("Total fun: " ++ show n) : map empName xs

main :: IO ()
main = readFile "company.txt" >>= putStrLn . formatGL . maxFun . read
