{-# LANGUAGE LambdaCase, GeneralizedNewtypeDeriving, TupleSections #-}

import Data.Semigroup (Max(..),stimes, Semigroup(..))
import Data.Monoid hiding ((<>))
import Data.Vector ((//),(!),Vector,toList)
import qualified Data.Vector as V (replicate)
import Control.Monad
import Control.Monad.Identity

type Stack = [Int]
type Memory = Vector Int

memSize = 4

data VM a = VM { stack :: Stack
               , status :: Maybe String
               , memory :: Memory
               , journal :: a }
            deriving Show

mkVM = VM mempty mempty (V.replicate memSize 0)

setStack  x (VM _ st m l) = pure $ VM x st m l

setStatus st (VM s _ m l) = pure $ VM s st m l

setMemory m (VM s st _ l) = pure $ VM s st m l

addRecord x (VM s st m l) = VM s st m (x<>l)

------------------------------------------------------------

data Code = IF [Code] [Code]
          | REP [Code]
          | WHILE [Code] [Code]
          | PUT Int | GET Int
          | PUSH Int | POP | DUP | SWAP | EXCH
          | INC | DEC | NEG
          | ADD | MUL | SUB | DIV | MOD
          | EQL | LTH | GTH | NEQ
          | ASK | PRT | PRTS String | FORK [Code] [Code]
          deriving (Read, Show)

newtype ActionM m a = ActionM {runActionM :: a -> m a}

instance Monad m => Semigroup (ActionM m a) where
  ActionM f <> ActionM g = ActionM (f >=> g)
instance Monad m => Monoid (ActionM m a) where
  mempty = ActionM pure

newtype ProgramM m a = ProgramM { getProgramM :: ([Code], ActionM m (VM a)) }
  deriving (Semigroup, Monoid)

type ProgramM' m a = (VM a -> m (VM a)) -> ProgramM m a

program c f p = ProgramM . ([c],) . ActionM $
  \vm -> case status vm of
    Nothing -> p c =<< f (stack vm) vm
    m -> pure vm

programM c f p = ProgramM . ([c],) . ActionM $
  \vm -> case status vm of
    Nothing -> p c =<< f (memory vm, stack vm) vm
    m -> pure vm

run = runActionM . snd . getProgramM
toCode prog = fst . getProgramM $ prog pure

err m = setStatus . Just $ "Error : " ++ m

nop :: Monad m => Code -> VM a -> m (VM a)
nop = const pure

exec prog = run (prog nop) (mkVM ())

execLog p prog = run (prog $ \c -> \vm -> pure $ addRecord (p c vm) vm) (mkVM mempty)

f &&& g = \c -> \r -> (f c r, g c r)

logStack _ vm   = [stack vm]
logStackUsed _ = Max . length . stack
logSteps _     = const (Sum 1)
logCode c _   = [c]

logRun com vm = [pad 10 c ++ "| " ++ pad 20 s ++ "| " ++ m]
  where c = show com
        m = unwords $ show <$> toList (memory vm)
        s = unwords $ show <$> stack vm
        pad n x = take n (x ++ repeat ' ')

debug p = unlines . reverse . journal $ execLog logRun p
 
------------------------------------------------------------
pop,dup,swap,exch :: Monad m => ProgramM' m a
put,get,push :: Monad m => Int -> ProgramM' m a
add,mul,sub,frac,modulo,inc,dec,neg :: Monad m => ProgramM' m a
eq,neq,lt,gt :: Monad m => ProgramM' m a

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

put i = indexed (PUT i) i $
    \case (m, x:s) -> setStack s <=< setMemory (m // [(i,x)])
          _ -> err "put expected an argument"

get i = indexed (GET i) i $ \(m, s) -> setStack ((m ! i) : s)

indexed c i f = programM c $ if (i < 0 || i >= memSize)
                             then const $ err "index in [0,16]"
                             else f

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
modulo = app2 MOD (flip mod)
neg = app1 NEG (\x -> -x)
inc = app1 INC (\x -> x+1)
dec = app1 DEC (\x -> x-1)
eq = app2 EQL (\x -> \y -> if (x == y) then 1 else 0)
neq = app2 NEQ (\x -> \y -> if (x /= y) then 1 else 0)
lt = app2 LTH (\x -> \y -> if (x > y) then 1 else 0)
gt = app2 GTH (\x -> \y -> if (x < y) then 1 else 0)

proceed p prog s = run (prog p) <=< setStack s

rep body p = program (REP (toCode body)) go pure
  where go (n:s) = if n >= 0
                   then proceed p (stimes n body) s
                   else err "rep expected positive argument."
        go _ = err "rep expected an argument."

branch br1 br2 p = program (IF (toCode br1) (toCode br2)) go pure
   where go (x:s) = proceed p (if (x /= 0) then br1 else br2) s
         go _ = err "branch expected an argument."

while test body p = program (WHILE (toCode test) (toCode body)) (const go) pure
  where go vm = do res <- proceed p test (stack vm) vm
                   case (stack res) of
                     0:s -> proceed p mempty s res
                     _:s -> go =<< proceed p body s res
                     _ -> err "while expected an argument." vm


ask, prt :: ProgramM' IO a
ask = program ASK $
  \case s -> \vm -> do x <- getLine
                       setStack (read x:s) vm

prt = program PRT $
  \case x:s -> \vm -> print x >> return vm
        _ -> err "PRT expected an argument"

prtS s = program (PRTS s) $
  const $ \vm -> print s >> return vm

fork :: ProgramM' [] a -> ProgramM' [] a -> ProgramM' [] a
fork br1 br2 p = program (FORK (toCode br1) (toCode br2)) (const go) pure
  where go = run (br1 p) <> run (br2 p)

------------------------------------------------------------

fromCode :: [Code] -> ProgramM' Identity a
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
    MOD -> modulo
    EQL -> eq
    LTH -> lt
    GTH -> gt
    NEQ -> neq
    NEG -> neg
    _ -> mempty

fromCodeIO :: [Code] -> ProgramM' IO a
fromCodeIO = foldMap $  
  \case
    IF b1 b2 -> branch (fromCodeIO b1) (fromCodeIO b2)
    REP p -> rep (fromCodeIO p)
    WHILE t b -> while (fromCodeIO t) (fromCodeIO b)
    ASK -> ask
    PRT -> ask
    PRTS s -> prtS s
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
    MOD -> modulo
    EQL -> eq
    LTH -> lt
    GTH -> gt
    NEQ -> neq
    NEG -> neg
    _ -> mempty

fromCodeList :: [Code] -> ProgramM' [] a
fromCodeList = foldMap $  
  \case
    IF b1 b2 -> branch (fromCodeList b1) (fromCodeList b2)
    REP p -> rep (fromCodeList p)
    WHILE t b -> while (fromCodeList t) (fromCodeList b)
    FORK b1 b2 -> fork (fromCodeList b1) (fromCodeList b2)
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
    MOD -> modulo
    EQL -> eq
    LTH -> lt
    GTH -> gt
    NEQ -> neq
    NEG -> neg
    _ -> mempty
    
------------------------------------------------------------
fact,range,fact1,fact2,fact3,copy2,gcd1,pow :: Monad m => ProgramM' m a


fact = dup <> push 2 <> lt <> branch (push 1) (dup <> dec <> fact) <> mul

fact1 = push 1 <> swap
        <> while (dup <> push 1 <> gt)
        (
          swap <> exch <> mul <> swap <> dec
        )
        <> pop

range = exch <> sub <> dup<> push 0 <> gt <> branch (rep (dup <> inc)) (neg <> rep (dup <> dec))

fact2 = inc <> push 1 <> swap <> range <> dec <> dec <> rep mul

fact3 = dup <> put 0 <> dup <> dec <> rep (dec <> dup <> get 0 <> mul <> put 0) <> get 0 <> swap <> pop

copy2 = exch <> exch

ioprog :: ProgramM' IO a
ioprog = prtS "first number" <> ask
         <> prtS "second number" <> ask
         <> rep (prt <> dup <> inc)
         <> prt

gcd1 = while (copy2 <> neq) 
       (
         copy2 <> lt <> branch mempty (swap) <> exch <> sub
       ) 
       <> pop

pow = swap <> put 0 <> push 1 <> put 1 <>
      while (dup <> push 0 <> gt)
      (
        dup <> push 2 <> modulo <>
        branch (dec <> get 0 <> dup <> get 1 <> mul <> put 1) (get 0) <>
        dup <> mul <> put 0 <>
        push 2 <> frac
      ) <>
      pop <> get 1
