{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Lib where

-- LANGUAGE: A-NORMAL FORM

data AExp = ALambda Lambda | AVar Var | ABool Bool | AInt Int

instance Show AExp where
        show (ALambda d) = show d
        show (AVar d) = show d
        show (ABool d) = if d then "#t" else "#f"
        show (AInt d) = show d
        -- `show (_ d) = show d` if I could but
        -- wildcard in data constructor's place is not allowed.

data CExp = If      AExp Exp Exp
          | Set     Var AExp
          | Proc    [AExp]
          | Letrec  [Bind] Exp
          | Callcc  AExp
          deriving Show

data Exp = Atomic   AExp
         | Complex  CExp
         | Let      Var Exp Exp
         -- deriving Show

instance Show Exp where
        show (Atomic d) = show d
        show _ = "todo"

defaultExp = Atomic $ AInt 0

type Var = String

data Lambda = Var :=> Exp
        deriving Show

data MLambda = [Var] ::=> Exp
        deriving Show

data Bind = Var := AExp
        deriving Show
bindFst :: Bind -> Var
bindFst b = v where v := _ = b
bindSnd b = a where _ := a = b

type Store = Addr -> Value
instance Show Store where show _ = "Store"
defaultStore :: Store
defaultStore _ = Undefined
-- defaultStore _ = error "Value undefined in Store for input Addr"  -- ?? is Undefined ok

data Kont = Letk Var Exp Env Kont | Halt
        deriving Show

data Value = Undefined | ValueVoid | ValueZ Int | ValueTrue | ValueFalse
           | ValueKont Kont | ValueClo (Lambda, Env) | ValueCloM (MLambda, Env)
        -- deriving (Eq, Show)
        deriving Show
type Addr = Int

-- Env and Store ?? typeclass
-- "as tuple" syntax.
(==>) :: a -> b -> (a, b)
(==>) x y = (x, y)
-- In x case return the anticipated y, otherwise inquire fn.
(//) :: Eq a => (a -> b) -> [(a, b)] -> (a -> b)
(//) fn [(x, y)] = \ input -> if (x == input) then y else fn(input)


-- THE MACHINE

type S = (Exp, Env, Store, Kont)
stateControl :: S -> Exp
stateControl s = c where (c, _, _, _) = s

applyproc :: (Value, [Value], Store, Kont) -> S
applyproc (ValueCloM (vs ::=> body, r), args, t, k)
      = (body, r, t, k)

readKont :: (Kont, Value, Store) -> S  -- applykont
-- readKont (Halt, _, _) = error "reading Halt continuation"
readKont ((Letk v e r k), value, t)
       = (e, r // [v ==> a], t // [a ==> value], k) where
       a = freshAddr 0
       freshAddr :: Addr -> Addr
       freshAddr addr = case t addr of
                Undefined  -> addr
                _          -> freshAddr $ succ addr

type Env = Var -> Addr
defaultEnv _ = error "Addr undefined in Env for input Var"

instance Show Env where show _ = "Env"

atomic :: (AExp, Env, Store) -> Value
atomic (AVar v, r, t)     = t $ r v
atomic (AInt z, _, _)     = ValueZ z
atomic (ABool b, _, _)    = if b then ValueTrue else ValueFalse
-- atomic (ALambda l, r, _)  = ValueClo (l, r)  ?? MLambda
atomic _     = ValueVoid

step :: S -> S

step (Atomic aexp, r, t, k) = readKont (k, atomic (aexp, r, t), t)

step ((Let v exp body), r, t, k)
   = (exp, r, t, (Letk v body r k))

step (Complex (If aexp tExp fExp), r, t, k)
   = case atomic (aexp, r, t) of
           ValueFalse  -> (fExp, r, t, k)
           _           -> (tExp, r, t, k)

step (Complex (Set v aexp), r, t, k)
   = readKont (k, ValueVoid, t // [r v ==> atomic (aexp, r, t)])

step (Complex (Proc exps), r, t, k)
   = applyproc (closure, args, t, k)
   where
        ai :: AExp -> Value
        ai arg = atomic (arg, r, t)
        -- [closure, args] = map ai $ exps  ?? split hd tail
        closure = ai $ head exps
        args = map ai $ tail exps

step (Complex (Letrec binds body), r, t, k)
   = (body, r', t', k)
   where
        addrs :: [Addr]
        addrs = alloc (length binds) t

        envs :: [(Var, Addr)]
        envs    = zip (map bindFst binds) addrs
        r' = foldl rExtend r envs

        stores :: [(Addr, Value)]
        ai :: AExp -> Value
        ai arg = atomic (arg, r', t)
        -- bindSndAsValue :: Bind -> Value  -- ?? nested map
        -- bindSndAsValue = bindValue where 

        stores  = zip addrs (map ai (map bindSnd binds))
        t' = foldl tExtend t stores

step (Complex (Callcc aexp), r, t, k)
   = applyproc (closure, [ValueKont k], t, k)
   where
        closure = atomic (aexp, r, t)

type Cap = Int
alloc :: Cap -> Store -> [Addr]
alloc cap store = addrs where
        h :: Addr -> [Addr] -> [Addr]
        h addr acc
          | length acc == cap = acc
          | otherwise = case store addr of
                Undefined  -> h (succ addr) $ acc ++ [addr]
                _          -> h (succ addr) acc
        addrs = h 0 []

-- test ok:
-- st1 :: Store
-- st1 1 = ValueTrue
-- st1 addr = defaultStore addr
-- teste = alloc 3 defaultStore
-- testd = alloc 3 st1

rExtend :: Env -> (Var, Addr) -> Var -> Addr
rExtend r (var, addr) = r' where
        r' var = addr
        r' _ = r var

tExtend :: Store -> (Addr, Value) -> Addr -> Value
tExtend t (addr, val) = t' where
        t' addr = val
        t' _ = t addr

inject :: Exp -> S
inject (e) = (e, defaultEnv, defaultStore, Halt)

eval :: S -> S
eval = until isFinal step 

isFinal :: S -> Bool
isFinal (_, _, _, Halt) = True
isFinal _ = False


-- TESTING

s0 = inject (Atomic (ABool False))
rez = stateControl s0
-- ghci> stateControl $ eval s0
-- #f
