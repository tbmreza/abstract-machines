{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Lib where

-- LANGUAGE: A-NORMAL FORM

data AExp = ALambda Lambda | AVar Var
          | ABool Bool | AInt Int
          | APrimAppl Prim [AExp]
        -- deriving Show
data Prim = Add | Sub | Mul | Eq deriving Show

instance Show AExp where
        show (ALambda d) = show d
        -- show (AVar d) = atomic d r t ...
        show (AVar d) = show d
        show (ABool d) = if d then "#t" else "#f"
        show (AInt d) = show d
        show a@(APrimAppl _ d) = show $ atomic (a, defaultEnv, defaultStore)
        show _ = "AExp todo.."
        -- `show (_ d) = show d` if I could but
        -- wildcard in data constructor's place is not allowed.

data Value = Undefined | ValueVoid | ValueZ Int | ValueTrue | ValueFalse
           | ValueKont Kont | ValueClo (Lambda, Env)
        -- deriving Show -- ??
instance Show Value where
        show Undefined = show Undefined
        show (ValueZ d) = show d
        show _ = "Value"

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
        show (Complex d) = show d
        show d = show d

defaultExp = Atomic $ ABool False

type Var = String

data Lambda = [Var] ::=> Exp
        deriving Show
ident :: Lambda
ident = ["x"] ::=> Atomic (AVar "x")

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
defaultS = (defaultExp, defaultEnv, defaultStore, Halt)
stateControl :: S -> Exp
stateControl s = c where (c, _, _, _) = s

applyproc :: (Value, [Value], Store, Kont) -> S
applyproc (ValueClo (vars ::=> body, r), args, t, k) = (body, r', t', k) where
        addrs = alloc (length vars) t
        r' = foldl rExtend r $ zip vars addrs
        t' = foldl tExtend t $ zip addrs args

applyproc _ = error "applying non-procedure Value"

readkont :: (Kont, Value, Store) -> S  -- applykont
readkont (Halt, value, _) = (Atomic ae, defaultEnv, defaultStore, Halt) where
        ae :: AExp
        ae = case value of
                ValueZ d -> AInt d
                ValueTrue -> ABool True
                ValueFalse -> ABool False
                _ -> AInt 0  -- ??

readkont ((Letk v e r k), value, t) = (e, r // [v ==> a], t // [a ==> value], k) where
        a = freshAddr 0
        freshAddr :: Addr -> Addr
        freshAddr addr = case t addr of
                Undefined  -> addr
                _          -> freshAddr $ succ addr

type Env = Var -> Addr
defaultEnv :: Env
defaultEnv _ = error "Addr undefined in Env for input Var"

instance Show Env where show _ = "Env"

atomic :: (AExp, Env, Store) -> Value
atomic (AVar v, r, t) = t $ r v
atomic (AInt z, _, _) = ValueZ z
atomic (ABool b, _, _) = if b then ValueTrue else ValueFalse

atomic (APrimAppl op aexps, r, t) = ValueZ z where
        fnadd :: AExp -> AExp -> AExp  -- ?? template haskell?
        fnadd a b = AInt new where
                [AInt acc, AInt arg] = [a, b]
                new = acc + arg
        fnsub a b = AInt new where
                [AInt acc, AInt arg] = [a, b]
                new = acc - arg
        fnmul a b = AInt new where
                [AInt acc, AInt arg] = [a, b]
                new = acc * arg

        -- Map primitive (prefix) ops to host language's (infix) ops.
        AInt z = case op of
                Add -> foldl fnadd (head aexps) (tail aexps)
                Sub -> foldl fnsub (head aexps) (tail aexps)
                Mul -> foldl fnmul (head aexps) (tail aexps)
                -- ??

atomic (ALambda l, r, _) = ValueClo (l, r)
atomic _ = ValueVoid

step :: S -> S

step (Atomic aexp, r, t, k) = readkont (k, atomic (aexp, r, t), t)

step ((Let var exp body), r, t, k)
   = (exp, r, t, (Letk var body r k))

step (Complex (If aexp tExp fExp), r, t, k)
   = case atomic (aexp, r, t) of
           ValueFalse  -> (fExp, r, t, k)
           _           -> (tExp, r, t, k)

step (Complex (Set v aexp), r, t, k)
   = readkont (k, ValueVoid, t // [r v ==> atomic (aexp, r, t)])

step (Complex (Proc aexps), r, t, k)
   = applyproc (closure, args, t, k)
   where
        asValue :: AExp -> Value
        asValue arg = atomic (arg, r, t)
        closure : args = map asValue aexps

step (Complex (Letrec binds body), r, t, k)
   = (body, r', t', k)
   where
        addrs = alloc (length binds) t
        r' = foldl rExtend r $ zip (map bindFst binds) addrs

        evalSnd :: Bind -> Value
        evalSnd b = atomic (bindSnd b, r', t)
        t' = foldl tExtend t $ zip addrs (map evalSnd binds)

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

injectWith :: Env -> Store -> Exp -> S
injectWith r t (e) = (e, r, t, Halt)

eval :: S -> S
eval = until isFinal step 

isFinal :: S -> Bool
isFinal (Atomic (AVar _), _, _, Halt) = False
isFinal (Atomic _, _, _, Halt) = True
isFinal _ = False


-- TESTING
-- ghci> stateControl $ eval s

s0 = inject (Atomic (ABool False))
-- #f

s1 = inject (Atomic (APrimAppl Add [AInt 2, AInt 13]))
-- 15

[s2, s3] = [
        inject (Complex (If (ABool True) (Atomic $ AInt 3) (Atomic $ AInt 2))),
        inject (Complex (If (ABool False) (Atomic $ AInt 3) (Atomic $ AInt 2))) ]
-- [3,2]

s4 = inject (Complex (Proc aexps)) where aexps = [ALambda ident, AInt 4]
-- 4

s5 = injectWith r5 t5 (Atomic $ AVar "b") where
        r5 :: Env
        r5 "b" = 12
        t5 :: Store
        t5 12 = ValueZ 6
-- 6

s6 = inject (Complex (Set "b" $ AInt 16))  -- ?? test in isolation without letrec

-- PICKUP
-- (check-eq? 6 (church->nat (church-compile
-- '(letrec ([len (lambda (lst)
--                  (if (null? lst)
--                      0
--                      (add1 (len (cdr lst)))))])
--    (len (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons '() '())))))))))))

s7 = inject (Complex (Letrec ["lr" := (AInt 31)]
                             (Atomic $ AVar "lr")))  -- 31 ok

testAll = map stateControl (map eval [s0, s1, s2, s3, s4, s5, s7])
