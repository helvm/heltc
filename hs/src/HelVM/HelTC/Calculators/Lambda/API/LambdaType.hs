module HelVM.HelTC.Calculators.Lambda.API.LambdaType where

defaultGeneratorType :: LambdaType
defaultGeneratorType = minBound

generatorTypes :: NonEmpty LambdaType
generatorTypes = universeNonEmpty

data LambdaType = MLC | SLC | UnLambda | Zot
  deriving stock (Bounded , Enum , Eq , Read , Show)
