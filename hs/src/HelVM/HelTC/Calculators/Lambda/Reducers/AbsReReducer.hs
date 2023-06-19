module HelVM.HelTC.Calculators.Lambda.Reducers.AbsReReducer (
  reduceAbsReForRootList,
  reduceAbsReForRoot,
) where

import           HelVM.HelTC.Calculators.Lambda.Lambda

import           HelVM.HelTC.Calculator.Value

reduceAbsReForRootList :: InstructionList -> InstructionList
reduceAbsReForRootList = fmap reduceAbsReForRoot

reduceAbsReForRoot :: Instruction -> Instruction
reduceAbsReForRoot (Def n f) = Def n $ reduceAbsRe f
reduceAbsReForRoot (Eval  f) = Eval  $ reduceAbsRe f

reduceAbsRe :: Lambda -> Lambda
reduceAbsRe (App f g)   = App (reduceAbsRe f) (reduceAbsRe g)
reduceAbsRe (Abs n f)   = Abs n $ reduceAbsRe f
reduceAbsRe (AbsRe p b) = lambdaFromAbsRe p b
reduceAbsRe       l     = l

lambdaFromAbsRe :: IdentifierList -> Lambda -> Lambda
lambdaFromAbsRe params body = reduceAbsRe $ foldr Abs body params
