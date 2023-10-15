module TRS where

data Term = Var String | Func String [Term]
  deriving Eq

type Rule = (Term, Term)
type TRS = [Rule]
