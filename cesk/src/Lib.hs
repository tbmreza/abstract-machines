{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Lib where

-- LANGUAGE: A-NORMAL FORM

data AExp = ALambda Lambda | AVar Var | ABool Bool | AInt Int
        deriving Show

data CExp = If (AExp, Exp, Exp)
        deriving Show

data Exp = Atomic AExp
         | Complex CExp
         | Let (Var, Exp, Exp)
         deriving Show

type Var = String

data Lambda = Var :=> Exp
        deriving Show


-- asChurchNum :: Int -> Lambda
-- asChurchNum n = "f" :=> Lam ("x" :=> (tally n)) where tally :: Int -> Exp
--                                                       tally 0 = Ref "x"
--                                                       tally n = (Ref "f") :@ (tally $ pred n)
-- asNat :: Exp -> Int
-- asNat e = h e 0 where
--         h (Ref _) x          = x
--         h (Lam (_ :=> t)) x  = h t x
--         h (_ :@ t) x         = h t (succ x)

-- Store extension σ[a↦value]  ??
type Store = Addr -> Value
data Kont = Letk (Var, Exp, Env, Kont) | Halt
        deriving Show

data Value = ValueVoid | ValueZ Int | ValueTrue | ValueFalse | ValueKont Kont | ValueClo (Lambda, Env)
type Addr = Int

-- Env
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

stateFrom :: (Kont, Value, Store) -> S  -- applykont
stateFrom (Letk (v, e, r, k), value, t)
            = (e, r // [v ==> a], t // [a ==> value], k) where a = 1  -- ??

type Env = Var -> Addr
instance Show Env where show _ = "Env"

atomic :: (AExp, Env, Store) -> Value
atomic (AVar v, r, t)     = t $ r v
atomic (AInt z, _, _)     = ValueZ z
atomic (ABool b, _, _)    = if b then ValueTrue else ValueFalse
atomic (ALambda l, r, _)  = ValueClo (l, r)
-- Primitive expressions ??

step :: S -> S

step (Atomic aexp, r, t, k) = stateFrom (k, atomic (aexp, r, t), t)

step (Let (v, exp, body), r, t, k)
   = (exp, r, t, Letk (v, body, r, k))

step (Complex (If (aexp, tExp, fExp)), r, t, k)
   = case atomic (aexp, r, t) of
           ValueFalse  -> (fExp, r, t, k)
           _           -> (tExp, r, t, k)

-- unchurch :: S -> Int
-- unchurch s = asNat $ stateControl s
--
-- inject :: Exp -> S
-- inject (e) = (e, defaultEnv, Mt)
--
-- isFinal :: S -> Bool
-- isFinal (Lam _, _, Mt) = True
-- isFinal _ = False
--
-- eval :: S -> S
-- eval = until isFinal step 
--
--
-- -- TESTING
--
-- s0 = inject (Ref "not_in_env")
-- -- ghci> stateControl $ eval s0
-- -- Lam ("" :=> Ref "not_in_env")
