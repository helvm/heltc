module HelVM.HelTC.Calculators.Legacy.API.CalculusType where

isLambda :: CalculusType -> Bool
isLambda Lambda = True
isLambda _      = False

inputCalculusType :: CalculusType
inputCalculusType = Zot

outputCalculusType :: CalculusType
outputCalculusType = Lambda

inputCalculusTypes :: [CalculusType]
--inputCalculusTypes = [Zot , UnLambda , Lambda]
inputCalculusTypes = [Zot]

calculusTypes :: [CalculusType]
calculusTypes = [Zot , UnLambda , Combinator , Lambda]

data CalculusType = Zot | UnLambda | Combinator | Lambda
  deriving stock (Bounded , Enum , Eq , Read , Show)
