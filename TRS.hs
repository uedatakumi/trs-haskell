module TRS where

data Term = Var String | Func String [Term]
  deriving (Eq, Show)

type Rule = (Term, Term)
type TRS = [Rule]
