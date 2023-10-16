module Matching where

import Substitution
import TRS

match' :: Substitution -> [(Term, Term)] -> Maybe Substitution
match' sigma []                        = Just sigma
match' sigma ((Func f ts, Func g us) : tus)
  | f == g                             = match' sigma (zip ts us ++ tus)
match' sigma ((Var x, u) : tus)
  | Just u' <- lookup x sigma, u == u' = match' sigma tus
  | Nothing <- lookup x sigma          = match' ((x, u) : sigma) tus
match' _ _                             = Nothing

match :: Term -> Term -> Maybe Substitution
match t u = match' [] [(t, u)]
