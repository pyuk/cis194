{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Calc where

import ExprT
import Parser
import qualified StackVM as S
import qualified Data.Map as M

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

evalStr' :: String -> Maybe Integer
evalStr' = fmap eval . parseExp Lit Add Mul

--exercise 3

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
   lit x = Lit x
   add x y = Add x y
   mul x y = Mul x y

--exercise 4

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7   = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
    lit x = x
    add x y = x + y
    mul x y = x * y

instance Expr Bool where
    lit x = if x <= 0 then False else True
    add x y = x || y
    mul x y = x && y

instance Ord MinMax where
    max (MinMax x) (MinMax y) = MinMax $ max x y
    min (MinMax x) (MinMax y) = MinMax $ min x y
    
instance Expr MinMax where
    lit x = MinMax x
    add x y = max x y
    mul x y = min x y

instance Expr Mod7 where
    lit x = Mod7 $ x `mod` 7
    add (Mod7 x) (Mod7 y) = Mod7 $ (x + y) `mod` 7
    mul (Mod7 x) (Mod7 y) = Mod7 $ (x * y) `mod` 7

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool    = testExp :: Maybe Bool
testMM      = testExp :: Maybe MinMax
testSat     = testExp :: Maybe Mod7

--exercise 5

{-instance Expr S.Program where
   lit x = [S.PushI x]
   add ([S.PushI x]) ([S.PushI y]) = [S.PushI (x + y)]
   mul ([S.PushI x]) ([S.PushI y]) = [S.PushI (x * y)]-}

instance Expr S.Program where
  lit x = [S.PushI x]
  add x y = x ++ y ++ [S.Add]
  mul x y = x ++ y ++ [S.Mul]

compile :: String -> Maybe S.Program
compile xs = parseExp lit add mul xs

--exercise 6

class HasVars a where
    var :: String -> a

data VarExprT = VarLit Integer
              | VarAdd VarExprT VarExprT
              | VarMul VarExprT VarExprT
              | Var String
    deriving (Show, Eq)

instance Expr VarExprT where
    lit x = VarLit x
    add x y = VarAdd x y
    mul x y = VarMul x y

instance HasVars VarExprT where
    var x = Var x

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var x = M.lookup x

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit x = \_ -> Just x
    add x y = \z -> if (x z) /= Nothing && (y z) /= Nothing 
                        then (x z) + (y z) 
                        else Nothing
    mul x y = \z -> if (x z) /= Nothing && (y z) /= Nothing 
                        then (x z) * (y z) 
                        else Nothing

withVars :: [(String, Integer)] 
         -> (M.Map String Integer -> Maybe Integer) 
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
