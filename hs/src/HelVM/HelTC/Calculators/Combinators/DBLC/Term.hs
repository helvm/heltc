module HelVM.HelTC.Calculators.Combinators.DBLC.Term where

-- Check if a variable occures freely in a term
freeIn :: Term -> Int -> Bool
freeIn (Var x)    n = x == n
freeIn (d :% d1)  n = freeIn d n || freeIn d1 n
freeIn (Lam t tp) n = freeIn t n || freeIn tp (1 + n)
freeIn _          _ = False

-- Increment free variables
quote :: Int -> Term -> Term
quote n (Var x)   = if x >= n then Var (1 + x) else Var x
quote n (Lam t d) = Lam (quote n t) (quote (1 + n) d)
quote n (d :% b)  = quote n d :% quote n b
quote _n x        = x

sub :: Term -> Int -> Term -> Term
sub s n (Var x) =
  case x `compare` n of
    GT -> Var (x - 1)
    EQ -> s
    LT -> Var x
sub s n (Lam t d) = Lam (sub s n t) (sub (quote 0 s) (1 + n) d)
sub s n (d :% b)  = sub s n d :% sub s n b
sub _ _ x         = x

-- Basic abstract syntax
infixl 9 :%
data Term
    = Lam Term Term
    | Var Int
    | Level Int
    | Term :% Term
    | U
    deriving stock (Eq, Show)
