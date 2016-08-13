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

treeFold :: (a -> b -> b) -> Tree a -> b
treeFold f (Node { rootLabel = a, subForest = (x:xs) }) = 
    a `f` (treeFold f (Node { rootLabel = (rootLabel x), subForest = xs }))

--exercise 3

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e g =
    (mconcat . map findFun $ g, mconcat . map findFun . map addBoss $ g)
    where findFun (a,b) = moreFun a b
          addBoss (a,b) = (glCons e a,glCons e b)
