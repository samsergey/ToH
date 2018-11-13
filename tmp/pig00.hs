{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Pig0 where

import Data.Semigroup (Max(..),stimes, Semigroup(..))
import Data.Monoid hiding ((<>))

type Stack = [Int]
type Processor = VM -> VM

data VM = VM { stack :: Stack }
  deriving Show

mkVM = VM mempty

setStack :: Stack -> Processor
setStack  x (VM _) = VM x

newtype Program = Program { getProgram :: Dual (Endo VM) }
  deriving (Semigroup, Monoid)

program :: (Stack -> Processor) -> Program
program f = Program . Dual . Endo $
  \vm -> (f (stack vm)) vm

run :: Program -> Processor
run = appEndo . getDual . getProgram

------------------------------------------------------------

pop = program $ 
  \case x:s -> setStack s
        _ -> error "pop expected an argument."

push x = program $ \s -> setStack (x:s)

dup = program $ 
  \case x:s -> setStack (x:x:s)
        _ -> error "dup expected an argument."

swap = program $ 
  \case x:y:s -> setStack (y:x:s)
        _ -> error "swap expected two arguments."

exch = program $ 
  \case x:y:s -> setStack (y:x:y:s)
        _ -> error "expected two arguments."

app1 n f = program $
  \case x:s -> setStack (f x:s)
        _ -> error $ "operation " ++ show n ++ " expected an argument"

app2 n f = program $
  \case x:y:s -> setStack (f x y:s)
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

proceed :: Program -> Stack -> Processor
proceed prog s = run prog . setStack s

rep :: Program -> Program
rep body = program go
  where go (n:s) = proceed (stimes n body) s
        go _ = error "rep expected an argument."

branch :: Program -> Program -> Program
branch br1 br2 = program go
   where go (x:s) = proceed (if (x /= 0) then br1 else br2) s
         go _ = error "branch expected an argument."

while :: Program -> Program -> Program
while test body = program (const go) 
  where go vm = let res = proceed test (stack vm) vm
          in case (stack res) of
               0:s -> proceed mempty s res
               _:s -> go $ proceed body s res
               _ -> error "while expected an argument." vm

------------------------------------------------------------

fact = dup <> push 2 <> lt <> branch (push 1) (dup <> dec <> fact) <> mul

fact1 = push 1 <> swap <> while (dup <> push 1 <> gt) (swap <> exch <> mul <> swap <> dec) <> pop

range = exch <> sub <> rep (dup <> inc)

fact2 = dec <> push 2 <> swap <> range <> push 3 <> sub <> rep mul

-- -- fact3 = dup <> put 0 <> dup <> dec <> rep (dec <> dup <> get 0 <> mul <> put 0) <> get 0 <> swap <> pop

copy2 = exch <> exch

gcd1 = while (copy2 <> neq) (copy2 <> lt <> branch (mempty) (swap) <> exch <> sub) <> pop

------------------------------------------------------------

