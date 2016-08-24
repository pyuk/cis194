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
  | otherwise = findBattle <$> (replicateM x die) <*> (replicateM y die)
  where findBattle xs ys = battle' (reverse  . sort $ xs) (reverse . sort $ ys)
        battle' (a:c:as) (b:d:bs) =
          case compare a b of
            GT -> case compare c d of
                    GT -> Battlefield x (y-2)
                    LT -> Battlefield (x-1) (y-1)
                    EQ -> Battlefield (x-1) (y-1)
            LT -> case compare c d of
                    GT -> Battlefield (x-1) (y-1)
                    LT -> Battlefield (x-2) y
                    EQ -> Battlefield (x-2) y
            EQ -> case compare c d of
                    GT -> Battlefield (x-1) (y-1)
                    LT -> Battlefield (x-2) y
                    EQ -> Battlefield (x-2) y