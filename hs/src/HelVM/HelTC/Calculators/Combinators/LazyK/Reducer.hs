module HelVM.HelTC.Calculators.Combinators.LazyK.Reducer (
  reduce,
  flippedApply,
  apply,
) where

import           HelVM.HelTC.Calculators.Combinators.LazyK.Combinator

reduce :: Combinator -> Combinator
reduce (App x y) = reduce x `apply` reduce y
reduce  x        = x

flippedApply :: Combinator -> Combinator -> Combinator
flippedApply = flip apply

apply :: Combinator -> Combinator -> Combinator
apply (S `App` x `App` y) z = apply x z `apply` apply y z
apply (App K x) _           = x
apply I x                   = x
apply Succ (Number x)       = Number $! x + 1
apply Succ x                = error $ "attempted to apply inc to a non-number " <> show x
apply f x                   = App f x
