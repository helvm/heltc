module HelVM.HelTC.Calculators.Combinators.API.CombinatorsType where

defaultCombinatorsType :: CombinatorsType
defaultCombinatorsType = minBound

combinatorsTypes :: NonEmpty CombinatorsType
combinatorsTypes = universeNonEmpty

data CombinatorsType = LazyK | Zot
  deriving stock (Bounded , Enum , Eq , Read , Show)
