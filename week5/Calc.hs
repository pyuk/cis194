module Calc where

import ExprT
import Parser

--exercise 1

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

--exercise 2

instance (Num a) => Num (Maybe a) where
    Just x + Just y = Just (x + y)
    Just x * Just y = Just (x * y)

eval' :: Maybe ExprT -> Maybe Integer
eval' Nothing = Nothing
eval' (Just (Lit x)) = Just x
eval' (Just (Add x y)) = eval' (Just x) + eval' (Just y)
eval' (Just (Mul x y)) = eval' (Just x) * eval' (Just y)

evalStr :: String -> Maybe Integer
evalStr xs = eval' $ parseExp Lit Add Mul xs

--exercise 3

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
   lit x = Lit x
   add x y = Add x y
   mul x y = Mul x y
