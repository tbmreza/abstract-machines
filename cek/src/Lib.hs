{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Lib where

data Exp = Ref Var
         | Lam Lambda
         | Exp :@ Exp
         deriving Show

type Env = Var -> D
instance Show Env where show _ = "Env"
defaultEnv :: Env
defaultEnv var = Clo (thunk, \x -> defaultD) where thunk = "" :=> Ref var

data Kont = Mt
          | Ar (Exp, Env, Kont)
          | Fn (Lambda, Env, Kont)
          deriving Show

type Var = String

data D = Clo (Lambda, Env) deriving Show
defaultD = Clo (c0, defaultEnv)

data Lambda = Var :=> Exp
        deriving Show

-- mod lambda calc codec begin
ident = "x" :=> Ref "x"
c0    = "f" :=> Lam ident
fx    = (Ref "f") :@ (Ref "x")
c1    = "f" :=> Lam ("x" :=> fx)

asNat e = h e 0 where
        h (Ref _) x          = x
        h (Lam (_ :=> t)) x  = h t x
        h (_ :@ t) x         = h t (succ x)

-- test
n0 = asNat $ Lam c0
n1 = asNat $ Lam c1

-- mod lambda calc codec end

type S = (Exp, Env, Kont)
stateControl :: S -> Exp
stateControl s = c where (c, _, _) = s

step :: S -> S

step (Ref x, r, k)
   = (Lam v, r', k) where Clo (v, r') = r(x) 

step (f :@ e, r, k)
   = (f, r, Ar(e, r, k))

step (Lam v, r, Ar(e, r', k))
   = (e, r', Fn(v, r, k))

step (Lam v, r, Fn(x :=> e, r', k))
   = (e, r' // [x ==> Clo(v, r)], k) where
        -- asTuple syntax.
        (==>) :: a -> b -> (a, b)
        (==>) x y = (x, y)
        -- Anticipate Fn's x. In x case return the anticipated y, otherwise inquire r'.
        (//) :: Eq a => (a -> b) -> [(a, b)] -> (a -> b)
        (//) r' [(x, y)] = \ input -> if (x == input) then y else r'(input)


s0 = (Ref "not_in_env", defaultEnv, Mt)

r1 :: Env
r1 "one" = Clo (c1, defaultEnv)
s1 = (Ref "one", r1, Mt)

s2 = (Lam c1, defaultEnv, Mt)
-- asNat $ stateControl s2

-- s3 = (app, defaultEnv, Mt) where app = Lam ident :@ Lam c1
s3 = (app, defaultEnv, Mt) where app = Lam ident :@ Lam c0
-- asNat $ stateControl $ step $ step s3
