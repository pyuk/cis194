{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -Wall #-}
module Fibonacci where

--exercise 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

--exercise 2

fibs2 :: [Integer]
fibs2 = [0,1] ++ fibs2' [0,1]
    where fibs2' (x:y:_) = (x + y) : fibs2' (y : [x + y])

--exercise 3

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
    show (Cons n ns) = "Cons " ++ (show n) ++ " " ++ (show' 20 ns)
        where show' 0 (Cons n ns) = "Cons " ++ (show n)
              show' x (Cons n ns) = "Cons " ++ (show n) ++ " " ++ (show' (x - 1) ns)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

--exercise 4

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons n ns) = Cons (f n) (streamMap f ns)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f n = Cons n $ streamFromSeed f (f n)

--exercise 5

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons n ns) ls = Cons n $ interleaveStreams ls ns

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) (f 1)
    where f n = interleaveStreams (streamRepeat n) (f $ n + 1)

--exercise 6

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
    fromInteger n = Cons n (streamRepeat 0)
    negate (Cons n ns) = Cons (-n) (negate ns)
    (+) (Cons n ns) (Cons l ls) = Cons (n + l) (ns + ls)
    (*) (Cons n ns) cs@(Cons l ls) = Cons (n * l) $ (streamMap (*n) ls) + (ns * cs)

instance Fractional (Stream Integer) where
    (/) (Cons n ns) (Cons l ls) = q
        where q = Cons (n `div` l) $ streamMap (`div` l) (ns - (q * ls))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

--exercise 7

data Matrix = Matrix (Integer, Integer, Integer, Integer)

instance Num Matrix where
    (*) (Matrix (a1,a2,a3,a4)) (Matrix (x1,x2,x3,x4)) = 
        Matrix (a1*x1+a2*x3, a1*x2+a2*x4, a3*x1+a4*x3, a3*x2+a4*x4)

fib4 :: Integer -> Integer
fib4 n
    | n == 0 = 0
    | n == 1 = 1
    | even n = b $ (f ^ (n `div` 2)) ^ 2
    | otherwise = b $ f * (f ^ ((n-1) `div` 2))^2
    where f = Matrix (1,1,1,0)
          b (Matrix (_,x,_,_)) = x
