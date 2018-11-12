{-# LANGUAGE LambdaCase, GeneralizedNewtypeDeriving #-}

import Data.Semigroup (Max(..))
import Data.Monoid
import Data.Vector ((//),(!),Vector)
import qualified Data.Vector as V (replicate)

memSize = 8

data VM = VM { memory :: Vector Int
             , stack :: [Int] }
          deriving Show

setMemory m  (VM _ s)  = VM m s
setStack  s  (VM m _)  = VM m s
mkVM = VM (V.replicate memSize 0) mempty

newtype Program = Program { getProgram :: Dual (Endo VM) }
  deriving (Monoid)

processor f = Program $ Dual $ Endo $
  \vm -> f (memory vm, stack vm) vm
run = appEndo . getDual . getProgram

------------------------------------------------------------

pop = processor $ 
  \case (_,x:s) -> setStack s
        _ -> error "!!!"

push x = processor $ \(_,s) -> setStack (x:s)

put i = processor $
  \case (m,x:s) -> setStack s . setMemory (m // [(i,x)])
        _ -> error "!!!"

get i = processor $ \(m,s) -> setStack ((m ! i) : s)

dup = processor $ 
  \case (_,x:s) -> setStack (x:x:s)
        _ -> error "!!!"

swap = processor $ 
  \case (_,x:y:s) -> setStack (y:x:s)
        _ -> error "!!!"

exch = processor $ 
  \case (_,x:y:s) -> setStack (y:x:y:s)
        _ -> error "!!!"

app1 n f = processor $
  \case (_,x:s) -> setStack (f x:s)
        _ -> error $ "operation " ++ show n ++ " expected an argument"

app2 n f = processor $
  \case (_,x:y:s) -> setStack (f x y:s)
        _ -> error $ "operation " ++ show n ++ " expected two arguments"

add = app2 "add" (+)
sub = app2 "sub" (flip (-))
mul = app2 "mul" (*)
frac = app2 "frac" (flip div)
neg = app1 "neg" (\x -> -x)
inc = app1 "inc" (\x -> x+1)
dec = app1 "dec" (\x -> x-1)
eq = app2 "eq" (\x -> \y -> if (x == y) then 1 else 0)
neq = app2 "neq" (\x -> \y -> if (x /= y) then 1 else 0)
lt = app2 "lt" (\x -> \y -> if (x > y) then 1 else 0)
gt = app2 "gt" (\x -> \y -> if (x < y) then 1 else 0)

proceed prog s = run prog . setStack s

rep body = processor go
  where go (_,n:s) = proceed (mconcat $ replicate n body) s
        go _ = error "rep expected an argument"

branch br1 br2 = processor go
  where go (_,x:s) = proceed (if (x /= 0) then br1 else br2) s
        go _ = error "branch expected an argument"

while test body = processor (const go) 
  where go vm = let res = proceed test (stack vm) vm
          in case (stack res) of
               0:s -> proceed mempty s res
               _:s -> go $ proceed body s res
               _ -> error "while expected an argument"

------------------------------------------------------------

fact = dup <> push 2 <> lt <> branch (push 1) (dup <> dec <> fact) <> mul

fact1 = push 1 <> swap <> while (dup <> push 1 <> gt) (swap <> exch <> mul <> swap <> dec) <> pop

range = rep (dup <> inc)

fact2 = dec <> push 2 <> swap <> range <> push 3 <> sub <> rep mul

fact3 = dup <> put 0 <> dup <> dec <> rep (dec <> dup <> get 0 <> mul <> put 0) <> get 0 <> swap <> pop

copy2 = exch <> exch

gcd1 = while (copy2 <> neq) (copy2 <> lt <> branch (mempty) (swap) <> exch <> sub) <> pop
 
