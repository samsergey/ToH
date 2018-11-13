{-# LANGUAGE LambdaCase,
GeneralizedNewtypeDeriving,
ExistentialQuantification,
TupleSections #-}

import Prelude hiding (log)
import Data.Semigroup (Max(..),stimes, Semigroup(..))
import Data.Monoid hiding ((<>))
import Data.Vector ((//),(!),Vector)
import qualified Data.Vector as V (replicate)

type Stack = [Int]
type Memory = Vector Int

memSize = 4

data VM a = VM { stack :: Stack
               , status :: Maybe String
               , memory :: Memory
               , journal :: a }
            deriving Show

mkVM = VM mempty mempty (V.replicate memSize 0)

setStack  x (VM _ st m l) = VM x st m l

setStatus st (VM s _ m l) = VM s st m l

setMemory m (VM s st _ l) = VM s st m l

addRecord x (VM s st m l) = VM s st m (x<>l)

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

execLog p prog = run (prog $ \vm -> addRecord (p vm) vm) (mkVM mempty)

f &&& g = \r -> (f r, g r)
  
logStack vm  = [stack vm]
logStackUsed = Max . length . stack
logSteps     = const (Sum 1)

logMemoryUsed :: VM a -> Max Int
logMemoryUsed = Max . getSum . count . memory
  where count = foldMap (\x -> if x == 0 then 0 else 1)

toCode prog = fst . getProgram $ prog id


  
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

rep body p = program (REP (toCode body)) go id
  where go (n:s) = if n >= 0
                   then proceed p (stimes n body) s
                   else err "rep expected positive argument."
        go _ = err "rep expected an argument."

branch br1 br2 p = program (IF (toCode br1) (toCode br2)) go id
   where go (x:s) = proceed p (if (x /= 0) then br1 else br2) s
         go _ = err "branch expected an argument."

while test body p = program (WHILE (toCode test) (toCode body)) (const go) id
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

-- fact = dup <> push 2 <> lt <> branch (push 1) (dup <> dec <> fact) <> mul

fact1 = push 1 <> swap <> while (dup <> push 1 <> gt) (swap <> exch <> mul <> swap <> dec) <> pop

range = exch <> sub <> dup<> push 0 <> gt <> branch (rep (dup <> inc)) (neg <> rep (dup <> dec))

fact2 = inc <> push 1 <> swap <> range <> dec <> dec <> rep mul

fact3 = dup <> put 0 <> dup <> dec <> rep (dec <> dup <> get 0 <> mul <> put 0) <> get 0 <> swap <> pop

copy2 = exch <> exch

gcd1 = while (copy2 <> neq) (copy2 <> lt <> branch mempty swap <> exch <> sub) <> pop

fact21 = fromCode . read $ "[DUP,PUT 0,DUP,DEC,REP [DEC,DUP,GET 0,MUL,PUT 0],GET 0,SWAP,POP]"

range1 = exch <> sub <> rep (dup <> inc)

------------------------------------------------------------

fromCode = foldMap $  
  \case
    IF b1 b2 -> branch (fromCode b1) (fromCode b2)
    REP p -> rep (fromCode p)
    WHILE t b -> while (fromCode t) (fromCode b)
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

data Req = Int :> Int
  deriving (Show,Eq)

instance Semigroup Req where
  i1 :> o1 <> i2 :> o2 = let a = i1 `max` (i2 - o1 + i1)
                         in a :> (a + o1 - i1 + o2 - i2)

instance Monoid Req where
  mappend = (<>)
  mempty = 0 :> 0

arity = arity' . toCode
  where
    arity' = foldMap $
      \case
        IF b1 b2 -> let i1:>o1 = arity' b1
                        i2:>o2 = arity' b2
                    in 1:>0 <> i1 `max` i2 :> o1 `min` o2
        REP p -> 1:>0
        WHILE t b -> arity' t <> 1:>0
        PUT _ -> 1:>0
        GET _ -> 0:>1
        PUSH _ -> 0:>1
        POP -> 1:>0
        DUP -> 1:>2
        SWAP -> 2:>2
        EXCH -> 2:>3
        INC -> 1:>1
        DEC -> 1:>1
        NEG -> 1:>1
        _   -> 2:>1


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

------------------------------------------------------------

pprint = unlines . printCode 0 . toCode
  where
    printCode n = foldMap f
      where
        f = \case
          IF b1 b2 -> print "IF" <> indent b1 <> print ":" <> indent b2
          REP p -> print "REP" <> indent p
          WHILE t b -> print "WHILE" <> indent t <> indent b
          c -> print $ show c

        print x = [stimes n "  " ++ x]
        indent = printCode (n+1)
