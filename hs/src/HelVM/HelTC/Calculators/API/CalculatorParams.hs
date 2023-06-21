module HelVM.HelTC.Calculators.API.CalculatorParams where

import           HelVM.HelTC.Calculators.Combinators.API.CombinatorsType
import           HelVM.HelTC.Calculators.Lambda.API.ILType
import           HelVM.HelTC.Calculators.Lambda.API.LambdaType

data CalculatorParams = Lambda LambdaType ILType | Combinators CombinatorsType
  deriving stock (Eq , Read , Show)

