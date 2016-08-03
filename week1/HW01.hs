--exercise 1

lastDigit :: Integer -> Integer
lastDigit x = x `mod` 10

dropLastDigit :: Integer -> Integer
dropLastDigit x = x `div` 10

--exercise 2

toRevDigits :: Integer -> [Integer]
toRevDigits x
    | x <= 0     = []
    | otherwise = lastDigit x : toRevDigits (dropLastDigit x)

--exercise 3

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:y:zs) = x : 2 * y : doubleEveryOther zs

--exercise 4

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
    | x < 10    = x + sumDigits xs
    | otherwise = lastDigit x + sumDigits (dropLastDigit x:xs)

--exercise 5

luhn :: Integer -> Bool
luhn x  
    | (sumDigits . doubleEveryOther . toRevDigits $ x) `mod` 10 == 0 = True
    | otherwise                                                      = False

--exercise 6

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b _ = [(a, b)]
hanoi x a b c = (hanoi (x - 1) a c b) ++ (hanoi 1 a b c) ++ (hanoi (x - 1) c b a)

--exercise 7

hanoi' :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi' 0 _ _ _ _ = []
hanoi' 1 a _ _ d = [(a, d)]
