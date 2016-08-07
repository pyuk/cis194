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
streamFromSeed f n = Cons n (streamFromSeed f (f n))

--exercise 5

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons n ns) (Cons l ls) = Cons n (Cons l (interleaveStreams ns ls))

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) (ruler' 1)
    where ruler' n = interleaveStreams (streamRepeat n) (ruler' $ n + 1)
