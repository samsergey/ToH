{-# LANGUAGE LambdaCase, GeneralizedNewtypeDeriving #-}

module Main where

import Data.Semigroup
import Data.Monoid
import Data.Vector ((//),(!),Vector)
import qualified Data.Vector as V (replicate)

memSize = 16

data VM = VM { memory :: Vector Int
             , stack :: [Int]
             , status :: Status
             , logg :: ([[Int]],Sum Int, Max Int)}
          deriving Show

setMemory m  (VM _ s st l)  = VM m s st l
setStack  s  (VM m _ st l)  = VM m s st l
setStatus st (VM m s _  l)  = VM m s st l
setLog    l  (VM m s st ls) = VM m s st (l<>ls)
mkVM = VM (V.replicate memSize 0) mempty Ok mempty

------------------------------------------------------------

data Status = Ok | Done String
  deriving Show

newtype Program a = Program { runProgram :: Dual (Endo VM) }
  deriving (Semigroup, Monoid)

run = appEndo . getDual . runProgram 
program =  Program . Dual . Endo -- . (writeLog .)

------------------------------------------------------------

processor f = program $
  \vm -> case status vm of
    Ok -> f (memory vm, stack vm) vm
    m -> vm

err = setStatus . Done . ("Error :" ++)
stop = processor $ setStatus . (const (Done "Ok"))
writeLog vm = setLog ([s], 1, Max (length s)) vm
  where s = stack vm

pop = processor $
  \case (_,x:s) -> setStack s
        _ -> err "pop expected an argument"

dup = processor $
  \case (_,x:s) -> setStack (x:x:s)
        _ -> err "dup expected an argument"

swap = processor $
  \case (_,x:y:s) -> setStack (y:x:s)
        _ -> err "swap expected two arguments"

push x = processor $ \(_,s) -> setStack (x:s)

app1 n f = processor $
  \case (_,x:s) -> setStack (f x:s)
        _ -> err $ "operation " ++ n ++ " expected an argument"

app2 n f = processor $
  \case (_,x:y:s) -> setStack (f x y:s)
        _ -> err $ "operation " ++ n ++ " expected two arguments"

add = app2 "add" (+)
sub = app2 "sub" (flip (-))
mul = app2 "mul" (*)
frac = app2 "frac" (flip div)
inc = app1 "inc" (\x -> x+1)
dec = app1 "dec" (\x -> x-1)
eq = app2 "eq" (\x -> \y -> if (x == y) then 1 else 0)
lt = app2 "lt" (\x -> \y -> if (x > y) then 1 else 0)
gt = app2 "gt" (\x -> \y -> if (x < y) then 1 else 0)

put i = indexed i $
    \case (m, x:s) -> setStack s . setMemory (m // [(i,x)])
          _ -> err "put expected an argument"

get i = indexed i $ \(m,s) -> setStack ((m ! i) : s)

indexed i p = processor $ if (i < 0 || i >= memSize)
                          then const $ err "index in [0,16]"
                          else p

branch p1 p2 = program $ 
  \vm -> case stack vm of
    (0:s) -> run p2 $ setStack s vm
    (1:s) -> run p1 $ setStack s vm
    _ -> err "branch expected an argument" vm

rep p = program $
  \vm -> case stack vm of
    (n:s) -> run (mconcat $ replicate n p) $ setStack s vm
    _ -> err "rep expected an argument" vm

prog = mconcat
br b1 b2 = branch (prog b1) (prog b2)

main = (print . stack) res
  where res = run (mconcat (replicate 100 prg)) mkVM
        prg = push 5 <> fact <> pop

------------------------------------------------------------
                 
fact = prog [dup, push 2, lt, br [push 1] [dup, dec, fact], mul]

range = rep (dup <> inc)

fact2 = dec <> push 2 <> swap <> range <> push 3 <> sub <> rep mul

fact3 = dup <> put 0 <> dup <> dec <> rep (dec <> dup <> get 0 <> mul <> put 0) <> get 0 <> swap <> pop
