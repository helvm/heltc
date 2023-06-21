module HelVM.HelTC.Calculators.API.CalculatorType where

defaultCalculatorType :: CalculatorType
defaultCalculatorType = minBound

calculatorTypes :: NonEmpty CalculatorType
calculatorTypes = universeNonEmpty

data CalculatorType = Lambda | Combinators
  deriving stock (Bounded , Enum , Eq , Read , Show)

