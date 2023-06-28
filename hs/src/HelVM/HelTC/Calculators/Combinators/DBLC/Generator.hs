module HelVM.HelTC.Calculators.Combinators.DBLC.Generator where

import           HelVM.HelTC.Calculators.Combinators.DBLC.Term

toBin :: Term -> String
toBin (Lam a b) = "010" ++ toBin a ++ toBin b
toBin (a :% b)  = "00" ++ toBin a ++ toBin b
toBin (Var i)   = numToBin i
toBin (Level i) = "011" ++ numToBin i
toBin U         = "0110"

numToBin :: Int -> String
numToBin 0 = "10"
numToBin i = '1' : numToBin (i-1)
