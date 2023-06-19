module HelVM.HelTC.Calculators.Lambda.Generators.Generator (
  generateCodeForIL,
  generateCodeForAbstract,
  generateCodeForLambda,
) where

import           HelVM.HelTC.Calculators.Lambda.API.ILType
import           HelVM.HelTC.Calculators.Lambda.API.LambdaType

import qualified HelVM.HelTC.Calculators.Lambda.Generators.Impl.MetaGenerator     as MetaGenerator
import qualified HelVM.HelTC.Calculators.Lambda.Generators.Impl.SymbolicGenerator as SymbolicGenerator
import qualified HelVM.HelTC.Calculators.Lambda.Generators.Impl.UnLambdaGenerator as UnLambdaGenerator
import qualified HelVM.HelTC.Calculators.Lambda.Generators.Impl.ZotGenerator      as ZotGenerator

import           HelVM.HelTC.Calculators.Lambda.Lambda

import           HelVM.HelIO.Control.Safe

generateCodeForIL :: ILType -> InstructionList -> Text
generateCodeForIL Meta     = MetaGenerator.generateCodeForIL
generateCodeForIL Symbolic = SymbolicGenerator.generateCodeForIL

generateCodeForAbstract :: ILType -> Lambda -> Text
generateCodeForAbstract Meta     = MetaGenerator.generateCodeForLambda
generateCodeForAbstract Symbolic = SymbolicGenerator.generateCodeForLambda

generateCodeForLambda :: MonadSafe m => LambdaType -> Lambda -> m Text
generateCodeForLambda MLC      = pure . MetaGenerator.generateCodeForLambda
generateCodeForLambda SLC      = pure . SymbolicGenerator.generateCodeForLambda
generateCodeForLambda UnLambda = UnLambdaGenerator.generateCodeForLambda
generateCodeForLambda Zot      = ZotGenerator.generateCodeForLambda
