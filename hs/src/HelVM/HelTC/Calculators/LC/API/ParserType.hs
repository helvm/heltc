module HelVM.HelTC.Calculators.LC.API.ParserType where

defaultParserType :: ParserType
defaultParserType = minBound

parserTypes :: NonEmpty ParserType
parserTypes = universeNonEmpty

data ParserType = Meta | Symbolic
  deriving stock (Bounded , Enum , Eq , Read , Show)
