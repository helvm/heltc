module HelVM.HelTC.Calculators.LC.API.GeneratorType where

defaultGeneratorType :: GeneratorType
defaultGeneratorType = minBound

generatorTypes :: NonEmpty GeneratorType
generatorTypes = universeNonEmpty

data GeneratorType = MLC | SLC | UnLambda | Zot
  deriving stock (Bounded , Enum , Eq , Read , Show)
