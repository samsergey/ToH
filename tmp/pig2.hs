{-# LANGUAGE LambdaCase, GeneralizedNewtypeDeriving, ExistentialQuantification #-}

import Prelude hiding (error, log)
import Data.Semigroup (Max(..),stimes)
import Data.Monoid
import Data.Vector ((//),(!),Vector)
import qualified Data.Vector as V (replicate)
import Data.Foldable

type Stack = [Int]
type Memory = Vector Int

<<<<<<< HEAD
memSize = 4
=======
memSize = 8
>>>>>>> 08385bba9d1de03cb0176ce5a1967f2544c67dd2

data VM a = VM { stack :: Stack
               , status :: Maybe String
               , memory :: Memory
               , journal :: a }
            deriving Show

mkVM x = VM mempty mempty (V.replicate memSize 0) x

setStack  x (VM _ st m j) = VM x st m j

setStatus st (VM s _ m j) = VM s st m j

setMemory m (VM s st _ j) = VM s st m j

addRecord x (VM s st m j) = VM s st m (x<>j)

------------------------------------------------------------

newtype Program a = Program { getProgram :: Dual (Endo (VM a)) }
  deriving (Semigroup, Monoid)

program f p = Program . Dual . Endo $
  \vm -> case status vm of
    Nothing -> p . (f (stack vm)) $ vm
    m -> vm

programM f p = Program . Dual . Endo $
  \vm -> case status vm of
    Nothing -> p . (f (memory vm, stack vm)) $ vm
    m -> vm

run = appEndo . getDual . getProgram

err m = setStatus . Just $ "Error : " ++ m

exec prog = run (prog id) (mkVM ())

execLog p prog = run (prog $ \vm -> addRecord (p vm) vm) (mkVM mempty)

f &&& g = \r -> (f r, g r)
  
logStack vm   = [stack vm]
logStackUsed  = Max . length . stack
logSteps      = const (Sum 1)

logMemoryUsed :: VM a -> Max Int
logMemoryUsed = Max . getSum . count . memory
  where count = foldMap (\x -> if x == 0 then 0 else 1)

------------------------------------------------------------

pop = program $ 
  \case x:s -> setStack s
        _ -> err "pop expected an argument."

push x = program $ \s -> setStack (x:s)

dup = program $ 
  \case x:s -> setStack (x:x:s)
        _ -> err "dup expected an argument."

swap = program $ 
  \case x:y:s -> setStack (y:x:s)
        _ -> err "swap expected two arguments."

exch = program $ 
  \case x:y:s -> setStack (y:x:y:s)
        _ -> err "expected two arguments."

app1 n f = program $
  \case x:s -> setStack (f x:s)
        _ -> err $ "operation " ++ show n ++ " expected an argument"

app2 n f = program $
  \case x:y:s -> setStack (f x y:s)
        _ -> err $ "operation " ++ show n ++ " expected two arguments"

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

proceed p prog s = run (prog p) . setStack s

rep body p = program go id
  where go (n:s) = proceed p (stimes n body) s
        go _ = err "rep expected an argument."

branch br1 br2 p = program go id
   where go (x:s) = proceed p (if (x /= 0) then br1 else br2) s
         go _ = err "branch expected an argument."

while test body p = program (const go) id
  where go vm = let res = proceed p test (stack vm) vm
          in case (stack res) of
               0:s -> proceed p mempty s res
               _:s -> go $ proceed p body s res
               _ -> err "while expected an argument." vm

put i = indexed i $
    \case (m, x:s) -> setStack s . setMemory (m // [(i,x)])
          _ -> err "put expected an argument"

get i = indexed i $ \(m, s) -> setStack ((m ! i) : s)

indexed i f = programM $ if (i < 0 || i >= memSize)
                         then const $ err "index in [0,16]"
                         else f

------------------------------------------------------------

fact = dup <> push 2 <> lt <> branch (push 1) (dup <> dec <> fact) <> mul

fact1 = push 1 <> swap <> while (dup <> push 1 <> gt) (swap <> exch <> mul <> swap <> dec) <> pop

range = rep (dup <> inc)

fact2 = dec <> push 2 <> swap <> range <> push 3 <> sub <> rep mul

fact3 = dup <> put 0 <> dup <> dec <> rep (dec <> dup <> get 0 <> mul <> put 0) <> get 0 <> swap <> pop

copy2 = exch <> exch

gcd1 = while (copy2 <> neq) (copy2 <> lt <> branch (mempty) (swap) <> exch <> sub) <> pop
