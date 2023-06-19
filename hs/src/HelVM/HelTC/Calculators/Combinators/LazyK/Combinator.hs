module HelVM.HelTC.Calculators.Combinators.LazyK.Combinator where

import qualified HelVM.HelTC.Calculators.Lambda.Lambda as L

import           Relude.Extra

fromLambda :: L.Lambda -> Combinator
fromLambda  L.S        = S
fromLambda  L.K        = K
fromLambda  L.I        = I
fromLambda (L.App f g) = fromLambda f `App` fromLambda g
fromLambda _           = error "error in fromLambda"

app4 :: Combinator -> Combinator -> Combinator -> Combinator -> Combinator
app4 c1 c2 c3 c4 = c1 `App` c2 `App` c3 `App` c4

app3 :: Combinator -> Combinator -> Combinator -> Combinator
app3 c1 c2 c3 = c1 `App` c2 `App` c3

foldlCombinator :: NonEmpty Combinator -> Combinator
foldlCombinator = foldl1' App

data Combinator =
    S
  | K
  | I
  | App Combinator Combinator
  | Succ
  | Number !Natural
  deriving stock (Eq , Read , Show)
