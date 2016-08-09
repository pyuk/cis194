module JoinList where

import Data.Monoid

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty          = Empty
tag (Append m _ _) = m
tag (Single m _)   = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty Empty = Empty
(+++) Empty l = l
(+++) r Empty = r
(+++) (Single m x) (Single m2 y) = Single (m <> m2) (x <> y)
(+++) (Append m r) (Append m2 l) = Append (m <> m2) (r +++ l)
