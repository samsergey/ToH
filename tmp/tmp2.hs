{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns, GADTs,StandaloneDeriving #-}
module Main where

------------------------------------------------------------

data Stepper a where
    Stepper :: Num a => a -> a -> Stepper a
    Nil :: Stepper a

deriving instance Show a => Show (Stepper a)

nextStep (Stepper x y) = (x, Stepper (x+y) y)

(^*) :: Num a => Stepper a -> a -> Stepper a
Stepper x y ^* f = Stepper (x * f) (y * f)

infixr 5 :-

pattern x :- xs <- (nextStep -> (x,xs))


instance Foldable Stepper where
    foldr f b (x :- xs) = f x (foldr f b xs)

------------------------------------------------------------


data Queue a = Queue { minKey :: !a
                     , minVal :: a
                     , rest :: Stepper a }
               deriving Show

singleton :: a -> a -> Queue a
singleton !k !v = Queue k v Nil

infixl 5 <+>
(<+>) :: Ord a => Queue a -> Queue a -> Queue a
(<+>) q1@(Queue x1 y1 ts1) q2@(Queue x2 y2 ts2)
  | x1 <= x2 = Queue x1 y1 (Stepper q2 ts1)
  | otherwise = Queue x2 y2 (Stepper q1 ts2)

mergeQs :: Ord a => Stepper a -> Queue a
mergeQs (            Nil) = errorWithoutStackTrace "tried to merge empty list"
mergeQs (       t :- Nil) = t
mergeQs (t1 :- t2 :- Nil) = t1 <+> t2
mergeQs (t1 :- t2 :- ts ) = t1 <+> t2 <+> mergeQs ts

insert :: Ord a => a -> a -> Queue a -> Queue a
insert !k !v = (<+>) (singleton k v)


primesq = 2 : sieve 3 (singleton 4 2)
  where
    adjust x q@(Queue y z qs)
        | x < y = q
        | otherwise = adjust x (insert (y + z) z (mergeQs qs))
    sieve x q
        | x < minKey q = x : sieve (x + 1) (insert (x * x) x q)
        | otherwise = sieve (x + 1) (adjust x q)

------------------------------------------------------------

main = print $ primesq !! 100000
