{-# LANGUAGE LambdaCase, GeneralizedNewtypeDeriving #-}

module Main where

import Data.Semigroup (Max(..))
import Data.Monoid
import Data.Vector ((//),(!),Vector)
import qualified Data.Vector as V (replicate)

memSize = 16

data VM = VM { memory :: Vector Int
             , stack :: [Int]
             , status :: Maybe String
             , logg :: ([[Int]],Sum Int, Max Int)}
          deriving Show

setMemory m  (VM _ s st l)  = VM m s st l
setStack  s  (VM m _ st l)  = VM m s st l
setStatus st (VM m s _  l)  = VM m s st l
setLog    l  (VM m s st ls) = VM m s st (l<>ls)
mkVM = VM (V.replicate memSize 0) mempty Nothing ([[]],0,Max 0)

------------------------------------------------------------

newtype Program = Program { getProgram :: (Dual (Endo VM), [Command]) }
  deriving (Monoid)

run = appEndo . getDual . fst . getProgram
code = snd . getProgram
mkProgram n p =  Program (Dual (Endo p), [n]) 

------------------------------------------------------------

processor n f = mkProgram n $
  \vm -> case status vm of
    Nothing -> writeLog $ f (memory vm, stack vm) vm
    _ -> vm

err = setStatus . Just . ("Error :" ++)
writeLog vm = setLog ([stack vm], 1, Max (length s)) vm
  where s = stack vm

pop = processor POP $
  \case (_,x:s) -> setStack s
        _ -> err "pop expected an argument"

dup = processor DUP $
  \case (_,x:s) -> setStack (x:x:s)
        _ -> err "dup expected an argument"

exch = processor EXCH $
  \case (_,x:y:s) -> setStack (y:x:y:s)
        _ -> err "exch expected two arguments"

swap = processor SWAP $
  \case (_,x:y:s) -> setStack (y:x:s)
        _ -> err "swap expected two arguments"

push x = processor (PUSH x) $ \(_,s) -> setStack (x:s)

app1 n f = processor n $
  \case (_,x:s) -> setStack (f x:s)
        _ -> err $ "operation " ++ show n ++ " expected an argument"

app2 n f = processor n $
  \case (_,x:y:s) -> setStack (f x y:s)
        _ -> err $ "operation " ++ show n ++ " expected two arguments"

add = app2 ADD (+)
sub = app2 SUB (flip (-))
mul = app2 MUL (*)
frac = app2 DIV (flip div)
inc = app1 INC (\x -> x+1)
dec = app1 DEC (\x -> x-1)
eq = app2 EQL (\x -> \y -> if (x == y) then 1 else 0)
neq = app2 EQL (\x -> \y -> if (x /= y) then 1 else 0)
lt = app2 LTH (\x -> \y -> if (x > y) then 1 else 0)
gt = app2 GTH (\x -> \y -> if (x < y) then 1 else 0)

put i = indexed (PUT i) i $
    \case (m, x:s) -> setStack s . setMemory (m // [(i,x)])
          _ -> err "put expected an argument"

get i = indexed (GET i) i $ \(m,s) -> setStack ((m ! i) : s)

indexed n i p = processor n $ if (i < 0 || i >= memSize)
                          then const $ err "index in [0,16]"
                          else p

proceed p s = run p . writeLog . setStack s

branch b1 b2 = mkProgram (IF (code b1) (code b2)) $ 
  \vm -> case stack vm of
    (0:s) -> proceed b2 s vm
    (_:s) -> proceed b1 s vm
    _ -> err "branch expected an argument" vm

rep body = mkProgram (REPEAT (code body)) $
  \vm -> case stack vm of
    (n:s) -> proceed (mconcat $ replicate n body) s vm
    _ -> err "rep expected an argument" vm

while test body = mkProgram (WHILE (code test) (code body)) $ go 
  where
    go vm = let res = proceed test (stack vm) vm
            in case stack res of
              (0:s) -> proceed mempty s res
              (_:s) -> go $ proceed body s res
              _ -> err "while expected an argument" res

main = do
--  mapM_ (putStrLn . unwords . map show) r
  print (stack res)
  print s
  print m
  where res = run prg mkVM
        (_,Sum s,Max m) = logg res
        prg = push 100 <> rep (push 20 <> fact <> push 20 <> fact1 <> push 20 <> fact2 <> push 20 <> fact3) <> push 400 <> rep pop

------------------------------------------------------------

data Command = IF [Command] [Command]
             | REPEAT [Command]
             | WHILE [Command] [Command]
             | PUT Int | GET Int
             | PUSH Int | POP | DUP | SWAP | EXCH
             | INC | DEC
             | ADD | MUL | SUB | DIV
             | EQL | LTH | GTH | NEQ
             deriving (Read, Show)

interprete =
  \case
    IF b1 b2 -> branch (readProg b1) (readProg b2)
    REPEAT p -> rep (readProg p)
    WHILE t b -> while (readProg t) (readProg b)
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

readProg = mconcat . map interprete

------------------------------------------------------------
 
fact = dup <> push 2 <> lt <> branch (push 1) (dup <> dec <> fact) <> mul

fact1 = push 1 <> swap <> while (dup <> push 1 <> gt) (swap <> exch <> mul <> swap <> dec) <> pop

range = rep (dup <> inc)

fact2 = dec <> push 2 <> swap <> range <> push 3 <> sub <> rep mul

fact3 = dup <> put 0 <> dup <> dec <> rep (dec <> dup <> get 0 <> mul <> put 0) <> get 0 <> swap <> pop

gcd1 = while (exch <> exch <> neq) (exch <> exch <> lt <> branch (mempty) (swap) <> exch <> sub) <> pop
