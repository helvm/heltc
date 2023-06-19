module HelVM.HelTC.Calculators.Legacy.Reducer (
  reduce,
) where

import           HelVM.HelTC.Calculators.Lambda.Lambda

import           HelVM.HelTC.Calculators.Legacy.API.CalculusType

import           HelVM.HelTC.Calculators.Lambda.Reducers.SkiReducer
import           HelVM.HelTC.Calculators.Legacy.Reducers.LambdaReducer

reduce :: CalculusType -> CalculusType -> Lambda -> Lambda
reduce inputType outputType = reduce' (isLambda inputType) (isLambda outputType)

reduce' :: Bool -> Bool -> Lambda -> Lambda
reduce' True  False = reduceLambda
reduce' False True  = reduceSki
reduce' _     _     = id
