{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Pig1 where

import Prelude hiding (error)
import Data.Semigroup (Max(..),stimes, Semigroup(..))
import Data.Monoid hiding ((<>))
import Data.Vector ((//),(!),Vector)
import qualified Data.Vector as V (replicate)

type Stack = [Int]
type Memory = Vector Int
type Processor = VM -> VM

memSize = 8

data VM = VM { stack :: Stack
             , status :: Maybe String
             , memory :: Memory }
          deriving Show

mkVM = VM mempty mempty (V.replicate memSize 0)

setStack :: Stack -> Processor
setStack  x (VM _ s m) = VM x s m

setStatus :: Maybe String -> Processor
setStatus x (VM s _ m) = VM s x m

setMemory :: Memory -> Processor
setMemory x (VM s st _) = VM s st x

newtype Program = Program { getProgram :: Dual (Endo VM) }
  deriving (Semigroup, Monoid)

program :: (Stack -> Processor) -> Program
program f = Program . Dual . Endo $
  \vm -> case status vm of
    Nothing -> (f (stack vm)) vm
    m -> vm

programM :: ((Memory, Stack) -> Processor) -> Program
programM f = Program . Dual . Endo $
  \vm -> case status vm of
    Nothing -> (f (memory vm, stack vm)) $ vm
    m -> vm

run :: Program -> Processor
run = appEndo . getDual . getProgram

error :: String -> Processor
error m = setStatus . Just $ "Error : " ++ m

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

put i = indexed i $
    \case (m, x:s) -> setStack s . setMemory (m // [(i,x)])
          _ -> error "put expected an argument"

get i = indexed i $ \(m, s) -> setStack ((m ! i) : s)

indexed i f = programM $ if (i < 0 || i >= memSize)
                         then const $ error "index in [0,16]"
                         else f

------------------------------------------------------------

fact = dup <> push 2 <> lt <> branch (push 1) (dup <> dec <> fact) <> mul

fact1 = push 1 <> swap <> while (dup <> push 1 <> gt) (swap <> exch <> mul <> swap <> dec) <> pop

range = rep (dup <> inc)

fact2 = dec <> push 2 <> swap <> range <> push 3 <> sub <> rep mul

fact3 = dup <> put 0 <> dup <> dec <> rep (dec <> dup <> get 0 <> mul <> put 0) <> get 0 <> swap <> pop

copy2 = exch <> exch

gcd1 = while (copy2 <> neq) (copy2 <> lt <> branch (mempty) (swap) <> exch <> sub) <> pop

------------------------------------------------------------

