{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List

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

aBattle :: Army -> Army -> DieValue -> DieValue -> DieValue -> DieValue -> DieValue -> (Int,Int)
aBattle x y a1 a2 a3 d1 d2 =
  case zipWith compare (reverse . sort $ [a1,a2,a3]) (reverse . sort $ [d1,d2]) of
    [LT,LT] -> (x - 2,y)
    [LT,GT] -> (x-1,y-1)
    [GT,LT] -> (x-1,y-1)
    [GT,GT] -> (x,y - 2)
    [EQ,EQ] -> (x - 2,y)
    [EQ,LT] -> (x - 2,y)
    [EQ,GT] -> (x-1,y-1)
    [LT,EQ] -> (x - 2,y)
    [GT,EQ] -> (x-1,y-1)

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield x 1) =
  die >>= \a1 -> die >>= \a2 ->
  die >>= \a3 -> die >>= \d ->
  return $ Battlefield
  (fst $ aBattle x 1 a1 a2 a3 d 0)
  (snd $ aBattle x 1 a1 a2 a3 d 0)
battle (Battlefield 2 y) =
  die >>= \a -> die >>= \d1 ->
  die >>= \d2 -> return $ Battlefield
  (fst $ aBattle 2 y a 0 0 d1 d2)
  (snd $ aBattle 2 y a 0 0 d2 d2)
battle (Battlefield 3 y) =
  die >>= \a1 -> die >>= \a2 ->
  die >>= \d1 -> die >>= \d2 ->
  return $ Battlefield
  (fst $ aBattle 3 y a1 a2 0 d1 d2)
  (snd $ aBattle 3 y a1 a2 0 d1 d2)
battle (Battlefield x y) =
  die >>= \a1 -> die >>= \a2 ->
  die >>= \a3 -> die >>= \d1 ->
  die >>= \d2 -> return $ Battlefield
  (fst $ aBattle x y a1 a2 a3 d1 d2)
  (snd $ aBattle x y a1 a2 a3 d1 d2)