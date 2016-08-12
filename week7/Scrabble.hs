{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scrabble where

--exercise 3

newtype Score = Score Int
    deriving (Show,Num)

--instance Num Score where
--    (+) (Score a) (Score b) = Score (a + b)

instance Monoid Score where
    mempty = Score 0
    mappend = (+)
    mconcat = foldr mappend mempty

fromScore :: Score -> Int
fromScore (Score x) = x

score :: Char -> Score
score a = let scores = [1,2,3,4,5,8,10]
              letters = ["aeilnorstu", "dg", "bcmp", "fhvwy", "k", "jx", "qz"]
              scoreZip = zip scores . map (a `elem`) $ letters
              findScore [] = 0
              findScore ((a, b):xs) | b == True = a | otherwise = findScore xs
          in  Score $ findScore scoreZip

scoreString :: String -> Score
--scoreString xs = foldr (\a b -> score a + b) (Score 0) xs
scoreString = mconcat . map score
