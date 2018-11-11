{-# LANGUAGE LambdaCase,
GeneralizedNewtypeDeriving,
ExistentialQuantification,
TupleSections #-}

import Prelude hiding (log)
import Data.Semigroup (Max(..),stimes)
import Data.Monoid
import Data.Vector ((//),(!),Vector)
import qualified Data.Vector as V (replicate)

type Stack = [Int]
type Memory = Vector Int

memSize = 16

data VM a = VM { stack :: Stack
               , status :: Maybe String
               , memory :: Memory
               , log :: a }
            deriving Show

mkVM x = VM mempty mempty (V.replicate memSize 0) x

setStack  x (VM _ st m l) = VM x st m l

setStatus st (VM s _ m l) = VM s st m l

setMemory m (VM s st _ l) = VM s st m l

addLog x (VM s st m l) = VM s st m (x<>l)

------------------------------------------------------------

data Code = IF [Code] [Code]
          | REP [Code]
          | WHILE [Code] [Code]
          | PUT Int | GET Int
          | PUSH Int | POP | DUP | SWAP | EXCH
          | INC | DEC | NEG
          | ADD | MUL | SUB | DIV
          | EQL | LTH | GTH | NEQ
          deriving (Read, Show)

newtype Program a = Program { getProgram :: ([Code], Dual (Endo (VM a))) }
  deriving (Semigroup, Monoid)

program c f p = Program . ([c],) . Dual . Endo $
  \vm -> case status vm of
    Nothing -> p . (f (stack vm)) $ vm
    m -> vm

programM c f p = Program . ([c],) . Dual . Endo $
  \vm -> case status vm of
    Nothing -> p . (f (memory vm, stack vm)) $ vm
    m -> vm

run = appEndo . getDual . snd . getProgram

err m = setStatus . Just $ "Error : " ++ m

exec prog = run (prog id) (mkVM Nothing)

execLog fl prog = run (prog $ \vm -> addLog (fl vm) vm) (mkVM mempty)

code p = fst . getProgram $ p id
  
------------------------------------------------------------

pop = program POP $ 
  \case x:s -> setStack s
        _ -> err "pop expected an argument."

push x = program (PUSH x) $ \s -> setStack (x:s)

dup = program DUP $ 
  \case x:s -> setStack (x:x:s)
        _ -> err "dup expected an argument."

swap = program SWAP $ 
  \case x:y:s -> setStack (y:x:s)
        _ -> err "swap expected two arguments."

exch = program EXCH $ 
  \case x:y:s -> setStack (y:x:y:s)
        _ -> err "expected two arguments."

app1 c f = program c $
  \case x:s -> setStack (f x:s)
        _ -> err $ "operation " ++ show c ++ " expected an argument"

app2 c f = program c $
  \case x:y:s -> setStack (f x y:s)
        _ -> err $ "operation " ++ show c ++ " expected two arguments"

add = app2 ADD (+)
sub = app2 SUB (flip (-))
mul = app2 MUL (*)
frac = app2 DIV (flip div)
neg = app1 NEG (\x -> -x)
inc = app1 INC (\x -> x+1)
dec = app1 DEC (\x -> x-1)
eq = app2 EQL (\x -> \y -> if (x == y) then 1 else 0)
neq = app2 NEQ (\x -> \y -> if (x /= y) then 1 else 0)
lt = app2 LTH (\x -> \y -> if (x > y) then 1 else 0)
gt = app2 GTH (\x -> \y -> if (x < y) then 1 else 0)

proceed p prog s = run (prog p) . setStack s

rep body p = program (REP (code body)) go id
  where go (n:s) = proceed p (stimes n body) s
        go _ = err "rep expected an argument."

branch br1 br2 p = program (IF (code br1) (code br2)) go id
   where go (x:s) = proceed p (if (x /= 0) then br1 else br2) s
         go _ = err "branch expected an argument."

while test body p = program (WHILE (code test) (code body)) (const go) id
  where go vm = let res = proceed p test (stack vm) vm
          in case (stack res) of
               0:s -> proceed p mempty s res
               _:s -> go $ proceed p body s res
               _ -> err "while expected an argument." vm

put i = indexed (PUT i) i $
    \case (m, x:s) -> setStack s . setMemory (m // [(i,x)])
          _ -> err "put expected an argument"

get i = indexed (GET i) i $ \(m, s) -> setStack ((m ! i) : s)

indexed c i f = programM c $ if (i < 0 || i >= memSize)
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

------------------------------------------------------------

readProg = readProg' . read
  where
    readProg' = foldMap $
      \case
        IF b1 b2 -> branch (readProg' b1) (readProg' b2)
        REP p -> rep (readProg' p)
        WHILE t b -> while (readProg' t) (readProg' b)
        PUT i -> put i
        GET i -> get i
        PUSH i -> push i
        POP -> pop
        DUP -> dup
        SWAP -> swap
        EXCH -> exch
        INC -> inc
        DEC -> dec
        ADD -> add
        MUL -> mul
        SUB -> sub
        DIV -> frac
        EQL -> eq
        LTH -> lt
        GTH -> gt
        NEQ -> neq
        NEG -> neg

------------------------------------------------------------
infix 7 :>

data Req = Int :> Int deriving (Show,Eq)

-- instance Eq Req where
--   (==) (a :> b) (c :> d) = (a==c) && (b==d)

instance Semigroup Req where
  n1 :> l1 <> n2 :> l2 = let a = (n1 `max` (n2 - l1 + n1) `max` n2)
                         in a :> (a + (l1 - n1) + (l2-n2))


instance Monoid Req where
  mempty = 0 :> 0

arity = process . code

process = foldMap f
  where
    f =
      \case
        IF b1 b2 -> undefined
        REP p -> undefined
        WHILE t b -> undefined
        PUT _ -> 1:>0
        GET _ -> 0:>1
        PUSH _ -> 0:>1
        POP -> 1:>0
        DUP -> 1:>2
        SWAP -> 2:>2
        EXCH -> 2:>3
        INC -> 1:>1
        DEC -> 1:>1
        ADD -> 2:>1
        MUL -> 2:>1
        SUB -> 2:>1
        DIV -> 2:>1
        EQL -> 2:>1
        LTH -> 2:>1
        GTH -> 2:>1
        NEQ -> 2:>1
        NEG -> 1:>1

tests = mapM_ print $
  [ 1:>2 <> 2:>3 == 1:>3
  , 0:>1 <> 1:>0 == 0:>0
  , 1:>0 <> 1:>0 == 2:>0
  , 1:>0 <> 2:>0 == 3:>0
  , 2:>0 <> 2:>0 == 4:>0
  , 1:>1 <> 1:>0 == 1:>0
  , 1:>1 <> 2:>0 == 2:>0
  , 2:>1 <> 2:>1 == 3:>1
  , 1:>0 <> 0:>1 == 1:>1
  , 1:>0 <> 1:>1 == 2:>1
  , 1:>1 <> 1:>0 == 1:>0
  , 1:>1 <> 1:>1 == 1:>1
  , 3:>0 <> 1:>1 == 4:>1
  , 0:>1 <> 0:>1 == 0:>2
  , 2:>3 <> 2:>3 == 2:>4
  , 1:>1 <> 3:>0 == 3:>0
  , 1:>3 <> 1:>3 == 1:>5
  , 0:>1 <> 2:>1 == 1:>1
  ]
