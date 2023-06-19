module HelVM.HelTC.Calculators.Lambda.Reducers.NaturalReducer (
  reduceNaturalForRootList,
  reduceNaturalForRoot,
) where

import           HelVM.HelTC.Calculators.Lambda.Lambda

reduceNaturalForRootList :: InstructionList -> InstructionList
reduceNaturalForRootList = fmap reduceNaturalForRoot

reduceNaturalForRoot :: Instruction -> Instruction
reduceNaturalForRoot (Def n f) = Def n $ reduceNatural f
reduceNaturalForRoot (Eval  f) = Eval  $ reduceNatural f

reduceNatural ::  Lambda -> Lambda
reduceNatural (App f g) = App (reduceNatural f) (reduceNatural g)
reduceNatural (Abs n f) = Abs n $ reduceNatural f
reduceNatural (Nat n)   = lambdaFromNatural n
reduceNatural      l    = l

lambdaFromNatural :: Natural -> Lambda
lambdaFromNatural 0 = falseVariable
lambdaFromNatural n = App succVariable (lambdaFromNatural $ n - 1)
