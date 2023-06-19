module HelVM.HelTC.Calculators.Lambda.Reducers.IntegerReducer (
  reduceIntegerForRootList,
  reduceIntegerForRoot,
) where

import           HelVM.HelTC.Calculators.Lambda.Lambda

import           HelVM.HelTC.Calculator.Value

reduceIntegerForRootList :: InstructionList -> InstructionList
reduceIntegerForRootList = fmap reduceIntegerForRoot

reduceIntegerForRoot :: Instruction -> Instruction
reduceIntegerForRoot (Def n f) = Def n $ reduceInteger f
reduceIntegerForRoot (Eval  f) = Eval  $ reduceInteger f

reduceInteger :: Lambda -> Lambda
reduceInteger (App f g) = App (reduceInteger f) (reduceInteger g)
reduceInteger (Abs n f) = Abs n $ reduceInteger f
reduceInteger (Int  sn) = lambdaFromInteger sn
reduceInteger        l  = l

lambdaFromInteger :: SignNatural -> Lambda
lambdaFromInteger (SN sign natural) = lambdaFromNonEmpty (pairVariable :| [lambdaFromSign sign , Nat natural])

lambdaFromSign :: Sign -> Lambda
lambdaFromSign Plus  = falseVariable
lambdaFromSign Minus = trueVariable
