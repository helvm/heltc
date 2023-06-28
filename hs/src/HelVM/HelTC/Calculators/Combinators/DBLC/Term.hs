module HelVM.HelTC.Calculators.Combinators.DBLC.Term where

-- Basic abstract syntax
infixl 9 :%
data Term
    = Lam Term Term
    | Var Int
    | Level Int
    | Term :% Term
    | U
    deriving stock (Eq, Show)
