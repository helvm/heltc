module HelVM.HelTC.Calculators.Legacy.CodeGenerator (
  generateCode,
) where

import           HelVM.HelTC.Calculators.Lambda.Lambda

import           HelVM.HelTC.Calculators.Legacy.API.CalculusType

import           HelVM.HelIO.Control.Safe

import qualified HelVM.HelTC.Calculators.Lambda.Generators.Impl.UnLambdaGenerator as UnLambdaGenerator
import qualified HelVM.HelTC.Calculators.Lambda.Generators.Impl.ZotGenerator      as ZotGenerator

import qualified HelVM.HelTC.Calculators.Lambda.Generators.Impl.HaskellGenerator  as HaskellGenerator

generateCode :: MonadSafe m => CalculusType -> Lambda -> m Text
generateCode Zot        = ZotGenerator.generateCodeForLambda
generateCode UnLambda   = UnLambdaGenerator.generateCodeForLambda
generateCode Combinator = pure . HaskellGenerator.generateCodeForLambda
generateCode Lambda     = pure . HaskellGenerator.generateCodeForLambda
