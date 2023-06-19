module HelVM.HelTC.Calculators.Combinators.LazyK.Constants where

import           HelVM.HelTC.Calculators.Combinators.LazyK.Combinator

bCombinator :: Combinator
bCombinator = app3 S appKS K

appSelfApp :: Combinator -> Combinator
appSelfApp = app4 S I I

-- | M combinator
selfApp :: Combinator
selfApp = app3 S I I

app3SI :: Combinator -> Combinator
app3SI = app3 S I

appKS :: Combinator
appKS = App K S

appK :: Combinator -> Combinator
appK = App K

-- | KI Combinator
false :: Combinator
false = App K I

true :: Combinator
true = K
