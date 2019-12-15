{-# LANGUAGE BangPatterns, DeriveFunctor #-}

module Main where

(\\) :: [Int] -> [Int] -> [Int]
[] \\ _ = []
xs \\ [] = xs
a@(x:xs) \\ b@(y:ys) = case compare x y of
  LT -> x : xs \\ b
  EQ -> xs \\ ys
  GT -> a \\ ys


sieve (p:ps) = p : sieve (ps \\ [p*p, p*p+p..])
primes = 2 : sieve [3,5..]


primes' = sieve' [2..]
sieve' (p:ps) = p : sieve' [ x | x <- ps, mod x p /= 0 ]

infixr 5 :-
data List a b  = Nil | (:-) {-# UNPACK #-} !(Queue a b) (List a b)

data Queue a b = Queue { minKey :: !a
                       , minVal :: b
                       , rest :: List a b }

singleton :: a -> b -> Queue a b
singleton !k !v = Queue k v Nil

infixl 5 <+>
(<+>) :: Ord a => Queue a b -> Queue a b -> Queue a b
(<+>) q1@(Queue x1 y1 ts1) q2@(Queue x2 y2 ts2)
  | x1 <= x2 = Queue x1 y1 (q2 :- ts1)
  | otherwise = Queue x2 y2 (q1 :- ts2)

mergeQs :: Ord a => List a b -> Queue a b
mergeQs (            Nil) = errorWithoutStackTrace "tried to merge empty list"
mergeQs (       t :- Nil) = t
mergeQs (t1 :- t2 :- Nil) = t1 <+> t2
mergeQs (t1 :- t2 :- ts ) = t1 <+> t2 <+> mergeQs ts

insert :: Ord a => a -> b -> Queue a b -> Queue a b
insert !k !v = (<+>) (singleton k v)




primesq = 2 : sieve 3 (singleton 4 2)
  where
    adjust x q@(Queue y z qs)
        | x < y = q
        | otherwise = adjust x (insert (y + z) z (mergeQs qs))
    sieve x q
        | x < minKey q = x : sieve (x + 1) (insert (x * x) x q)
        | otherwise = sieve (x + 1) (adjust x q)


main = print $ primesq !! 100000

newtype Parser a = Parser {unParse :: String -> [(String,a)]}
--  deriving Functor

instance Functor Parser where
  fmap f = Parser . (fmap . fmap . fmap) f . unParse


