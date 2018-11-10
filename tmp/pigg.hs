{-# LANGUAGE LambdaCase, GeneralizedNewtypeDeriving #-}

import Data.Semigroup (Max(..))
import Data.Monoid
import Data.Vector ((//),(!),Vector)
import qualified Data.Vector as V (replicate)

memSize = 16

-- v1
data VM = VM { memory :: Vector Int
             , stack :: [Int] }
          deriving Show

setMemory m  (VM _ s)  = VM m s
setStack  s  (VM m _)  = VM m s
mkVM = VM (V.replicate memSize 0) mempty

-- v2
-- data VM = VM { memory :: Vector Int
--              , stack :: [Int]
--              , status :: Maybe String}
--           deriving Show
-- setMemory m  (VM _ s st)  = VM m s st
-- setStack  s  (VM m _ st)  = VM m s st
-- setStatus st (VM m s _ )  = VM m s st
-- mkVM = VM (V.replicate memSize 0) mempty mempty

-- v3
-- data VM = VM { memory :: Vector Int
--              , stack :: [Int]
--              , status :: Maybe String
--              , logg :: ([[Int]],Sum Int, Max Int)}
--           deriving Show
-- setMemory m  (VM _ s st l)  = VM m s st l
-- setStack  s  (VM m _ st l)  = VM m s st l
-- setStatus st (VM m s _  l)  = VM m s st l
-- setLog    l  (VM m s st ls) = VM m s st (l<>ls)
-- mkVM = VM (V.replicate memSize 0) mempty Nothing ([[]],0,Max 0)

newtype Program = Program { getProgram :: Dual (Endo VM) }
  deriving (Monoid)

-- v1
processor f = Program $ Dual $ Endo $
  \vm -> f (memory vm, stack vm) vm
run = appEndo . getDual . getProgram
err m = error m

-- v2
--processor f = Program $ Dual $ Endo $
--  \vm -> case status vm of
--    Nothing -> f (memory vm, stack vm) vm
--    _ -> vm

-- v3
-- processor f p = Program $ Dual $ Endo $
--   \vm -> case status vm of
--     Nothing -> p $ f (memory vm, stack vm) vm
--     _ -> vm
-- 
-- exec p prg = appEndo . getDual . getProgram $ prg p
-- run = exec id
-- runLog = exec writeLog
-- writeLog vm = setLog ([s], 1, Max (length s)) vm
--   where s = stack vm

-- proceed p prog s = exec p prog . setStack s

------------------------------------------------------------

pop = processor $ 
  \case (_,x:s) -> setStack s
        _ -> err "!!!"

push x = processor $ \(_,s) -> setStack (x:s)

put i = processor $
  \case (m,x:s) -> setStack s . setMemory (m // [(i,x)])
        _ -> err "!!!"

get i = processor $ \(m,s) -> setStack ((m ! i) : s)

dup = processor $ 
  \case (_,x:s) -> setStack (x:x:s)
        _ -> err "!!!"

swap = processor $ 
  \case (_,x:y:s) -> setStack (y:x:s)
        _ -> err "!!!"

exch = processor $ 
  \case (_,x:y:s) -> setStack (y:x:y:s)
        _ -> err "!!!"

app1 f = processor $
  \case (_,x:s) -> setStack (f x:s)
        _ -> err $ "operation " ++ show n ++ " expected an argument"

app2 f = processor $
  \case (_,x:y:s) -> setStack (f x y:s)
        _ -> err $ "operation " ++ show n ++ " expected two arguments"

add = app2 (+)
sub = app2 (flip (-))
mul = app2 (*)
frac = app2 (flip div)
inc = app1 (\x -> x+1)
dec = app1 (\x -> x-1)
eq = app2 (\x -> \y -> if (x == y) then 1 else 0)
neq = app2 (\x -> \y -> if (x /= y) then 1 else 0)
lt = app2 (\x -> \y -> if (x > y) then 1 else 0)
gt = app2 (\x -> \y -> if (x < y) then 1 else 0)

-- v1,2
proceed prog s = run prog . setStack s

rep body = processor go
  where go (_,n:s) = proceed (mconcat $ replicate n body) s
        go _ = err "rep expected an argument"

branch br1 br2 = processor go
  where go (_,x:s) = proceed (if (x /= 0) then br1 else br2) s
        go _ = err "branch expected an argument"

while test body = processor (const go) 
  where go vm = let res = proceed test (stack vm) vm
          in case (stack res) of
               0:s -> proceed mempty s res
               _:s -> go $ proceed body s res
               _ -> err "while expected an argument"

-- v3
-- proceed p prog s = exec p prog . setStack s

-- rep body p = processor go id
--   where go (_,n:s) = proceed p (mconcat $ replicate n body) s
--         go _ = err "rep expected an argument"

-- branch br1 br2 p = processor go id
--   where go (_,x:s) = proceed p (if (x /= 0) then br1 else br2) s
--         go _ = err "branch expected an argument"

-- while test body p = processor (const go) id 
--   where go vm = let res = proceed p test (stack vm) vm
--           in case (stack res) of
--                0:s -> proceed p mempty s res
--                _:s -> go $ proceed p body s res
--                _ -> err "while expected an argument"

------------------------------------------------------------

fact = dup <> push 2 <> lt <> branch (push 1) (dup <> dec <> fact) <> mul

fact1 = push 1 <> swap <> while (dup <> push 1 <> gt) (swap <> exch <> mul <> swap <> dec) <> pop

range = rep (dup <> inc)

fact2 = dec <> push 2 <> swap <> range <> push 3 <> sub <> rep mul

fact3 = dup <> put 0 <> dup <> dec <> rep (dec <> dup <> get 0 <> mul <> put 0) <> get 0 <> swap <> pop

gcd1 = while (exch <> exch <> neq) (exch <> exch <> lt <> branch (mempty) (swap) <> exch <> sub) <> pop
