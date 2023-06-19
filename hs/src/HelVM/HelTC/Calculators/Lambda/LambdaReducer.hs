module HelVM.HelTC.Calculators.Lambda.LambdaReducer (
  reduceLambda,
) where

import           HelVM.HelTC.Calculators.Lambda.Reducers.AbsReReducer
import           HelVM.HelTC.Calculators.Lambda.Reducers.AbstractionReducer
import           HelVM.HelTC.Calculators.Lambda.Reducers.IntegerReducer
import           HelVM.HelTC.Calculators.Lambda.Reducers.ListReducer
import           HelVM.HelTC.Calculators.Lambda.Reducers.NaturalReducer
import           HelVM.HelTC.Calculators.Lambda.Reducers.StringReducer

import           HelVM.HelTC.Calculators.Lambda.Lambda

reduceLambda :: InstructionList -> InstructionList
reduceLambda = reduceAbstractionsForRootList . reduceNaturalForRootList . reduceIntegerForRootList . reduceListForRootList . reduceStringForRootList . reduceAbsReForRootList
