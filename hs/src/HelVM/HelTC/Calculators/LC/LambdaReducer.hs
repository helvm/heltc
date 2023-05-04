module HelVM.HelTC.Calculators.LC.LambdaReducer (
  reduceLambda,
) where

import           HelVM.HelTC.Calculators.LC.Reducers.AbsReReducer
import           HelVM.HelTC.Calculators.LC.Reducers.AbstractionReducer
import           HelVM.HelTC.Calculators.LC.Reducers.IntegerReducer
import           HelVM.HelTC.Calculators.LC.Reducers.ListReducer
import           HelVM.HelTC.Calculators.LC.Reducers.NaturalReducer
import           HelVM.HelTC.Calculators.LC.Reducers.StringReducer

import           HelVM.HelTC.Calculators.LC.Lambda

reduceLambda :: InstructionList -> InstructionList
reduceLambda = reduceAbstractionsForRootList . reduceNaturalForRootList . reduceIntegerForRootList . reduceListForRootList . reduceStringForRootList . reduceAbsReForRootList
