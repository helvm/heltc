module HelVM.HelTC.Calculators.Lambda.Reducers.ListReducer (
  reduceListForRootList,
  reduceListForRoot,
) where

import           HelVM.HelTC.Calculators.Lambda.Lambda

reduceListForRootList :: InstructionList -> InstructionList
reduceListForRootList = fmap reduceListForRoot

reduceListForRoot :: Instruction -> Instruction
reduceListForRoot (Def n f) = Def n $ reduceList f
reduceListForRoot (Eval  f) = Eval  $ reduceList f

reduceList :: Lambda -> Lambda
reduceList (App f g) = App (reduceList f) (reduceList g)
reduceList (Abs n f) = Abs n $ reduceList f
reduceList (List l)  = lambdaFromList l
reduceList       l   = l

lambdaFromList :: LambdaList -> Lambda
lambdaFromList = foldr appPairVariable falseVariable
