module HelVM.HelTC.Calculators.LC.API.ILType where

defaultParserType :: ILType
defaultParserType = minBound

parserTypes :: NonEmpty ILType
parserTypes = universeNonEmpty

data ILType = Meta | Symbolic
  deriving stock (Bounded , Enum , Eq , Read , Show)
