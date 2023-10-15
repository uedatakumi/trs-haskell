module Substitution where

import TRS

type Substitution = [(String, Term)]

substitute :: Term -> Substitution -> Term
substitute (Var x) sigma
  | Just t <- lookup x sigma = t
  | otherwise                = Var x
substitute (Func f ts) sigma = Func f [ substitute t sigma | t <- ts ]
