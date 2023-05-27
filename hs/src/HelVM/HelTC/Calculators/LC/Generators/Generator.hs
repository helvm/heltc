module HelVM.HelTC.Calculators.LC.Generators.Generator (
  generateCodeForIL,
  generateCodeForAbstract,
  generateCodeForLambda,
) where

import           HelVM.HelTC.Calculators.LC.API.ILType
import           HelVM.HelTC.Calculators.LC.API.LambdaType

import qualified HelVM.HelTC.Calculators.LC.Generators.Impl.MetaGenerator     as MetaGenerator
import qualified HelVM.HelTC.Calculators.LC.Generators.Impl.SymbolicGenerator as SymbolicGenerator
import qualified HelVM.HelTC.Calculators.LC.Generators.Impl.UnLambdaGenerator as UnLambdaGenerator
import qualified HelVM.HelTC.Calculators.LC.Generators.Impl.ZotGenerator      as ZotGenerator

import           HelVM.HelTC.Calculators.LC.Lambda

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
