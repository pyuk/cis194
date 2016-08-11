{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module JoinList where

import Data.Monoid
import Sized
import Scrabble
import Buffer

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

findSize :: (Sized b, Monoid b) => JoinList b a -> Int
findSize = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ n _ | n < 0 = Nothing
indexJ n (Single a b)
    | n == 0    = Just b
    | otherwise = Nothing
indexJ n (Append a l r) 
    | n < findSize l = indexJ n l
    | otherwise      = indexJ (n - findSize l) r

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ 0 l = l
dropJ _ Empty = Empty
dropJ n (Single _ _) = Empty
dropJ n (Append m l r) 
    | n <= findSize l = dropJ n l +++ r
    | otherwise       = dropJ (n - findSize l) r

takeJ :: (Sized b, Monoid b) => Int ->JoinList b a -> JoinList b a
takeJ 0 l = Empty
takeJ _ Empty = Empty
takeJ n l@(Single _ _) = l
takeJ n (Append a l r)
    | n <= findSize l = takeJ n l
    | otherwise       = l +++ takeJ (n - findSize l) r

--exercise 3

scoreLine :: String -> JoinList Score String
scoreLine xs = Single (scoreString xs) xs

--exercise 4

instance Buffer (JoinList (Score,Size) String) where
    toString (Single m a) = a
    toString (Append x r l) = toString r ++ toString r
    fromString a = Single (scoreString a,1) a
    line x j = indexJ x j
    replaceLine x xs j = takeJ x j +++ fromString xs +++ dropJ (x + 1) j
    numLines j = findSize j
    value j = fromScore . f . tag $ j
        where f (a,b) = a
