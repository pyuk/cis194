{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List
import Control.Monad

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving Show

--exercise 2

battle :: Battlefield -> Rand StdGen Battlefield
battle bf@(Battlefield x y)
  | x <= 1 || y <= 0 = return bf
  | otherwise = findBattle <$>
                replicateM (min 3 $ x - 1) die <*>
                replicateM (min 2 y) die
  where findBattle xs ys = battle' x y (reverse . sort $ xs) (reverse . sort $ ys)
        battle' x y [] _ = Battlefield x y
        battle' x y _ [] = Battlefield x y
        battle' x y (a:as) (b:bs)
          | a > b     = battle' x (y - 1) as bs
          | otherwise = battle' (x - 1) y as bs

--exercise 3

invade :: Battlefield -> Rand StdGen Battlefield
invade bf@(Battlefield x y)
  | x < 2 || y == 0 = return bf
  | otherwise       = battle bf >>= invade

--exercise 4

successProb :: Battlefield -> Rand StdGen Double
successProb = let getBattles = replicateM 1000 . invade
                  findProb = (/1000) . fromIntegral . length
                  applyFilter = filter $ (==0) . defenders
              in  fmap (findProb . applyFilter) . getBattles

--exercise 5

exactSuccessProb :: Battlefield -> Double
exactSuccessProb (Battlefield x y)
  | x <= 1 = 0
  | y <= 0 = 1
  | otherwise = 