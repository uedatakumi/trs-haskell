module Rewriting where

import Matching
import Substitution
import TRS

rewrite :: TRS -> Term -> Maybe Term
rewrite [] _                = Nothing
rewrite ((l, r) : rs) t
  | Just sigma <- match l t = Just (substitute r sigma)
  | otherwise               = rewrite rs t

nf :: TRS -> Term -> Term
nf _ (Var x)                 = Var x
nf trs (Func f ts)
  | Just u' <- rewrite trs u = nf trs u'
  | otherwise                = u
  where u = Func f [ nf trs t | t <- ts ]
