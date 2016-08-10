module JoinList where

import Data.Monoid
import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

--exercise 1

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Append m _ _) = m
tag (Single m _)   = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
--(+++) Empty Empty = Empty
--(+++) Empty r = r
--(+++) l Empty = l
(+++) l r = Append (tag l <> tag r) l r

--exercise 2

(!!?) :: [a] -> Int -> Maybe a
[]     !!? _ = Nothing
_      !!? i | i < 0 = Nothing
(x:xs) !!?  0 = Just x
(x:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ n _ | n < 0 = Nothing
indexJ n (Single a b)
    | n == 0    = Just b
    | otherwise = Nothing
indexJ n (Append a l r) 
    | findSize l > findSize r = indexJ (n - 1) l
    | otherwise               = indexJ (n - 1) r
    where findSize = getSize . size . tag

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ 0 l = l
dropJ _ Empty = Empty
dropJ n (Single _ _) = Empty
dropJ n (Append m l r) 

takeJ :: (Sized b, Monoid b) => Int ->JoinList b a -> JoinList b a
takeJ 0 l = Empty
takeJ _ Empty = Empty
takeJ n l@(Single _ _) = l
takeJ n (Append a l r)
