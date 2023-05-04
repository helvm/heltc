module HelVM.HelTC.Calculators.LC.Generators.LambdaGenerator (
  generateCodeForLambda,
  generateUnLambda,
  generateZot,
) where

import qualified HelVM.HelTC.Calculators.LC.Generators.Expression.MetaGenerator     as MetaGenerator
import qualified HelVM.HelTC.Calculators.LC.Generators.Expression.SymbolicGenerator as SymbolicGenerator

import           HelVM.HelTC.Calculators.LC.API.GeneratorType

import           HelVM.HelTC.Calculators.LC.Lambda

import           HelVM.HelIO.Control.Safe

generateCodeForLambda :: MonadSafe m => GeneratorType -> Lambda -> m Text
generateCodeForLambda MLC      = pure . MetaGenerator.generateCodeForLambda
generateCodeForLambda SLC      = pure . SymbolicGenerator.generateCodeForLambda
generateCodeForLambda UnLambda = generateUnLambda
generateCodeForLambda Zot      = generateZot

-- | UnLambda
generateUnLambda :: MonadSafe m => Lambda -> m Text
generateUnLambda l = appendError (show l) $ generateUnLambdaFix l

generateUnLambdaFix :: MonadSafe m => Lambda -> m Text
generateUnLambdaFix S         = pure "s"
generateUnLambdaFix K         = pure "k"
generateUnLambdaFix I         = pure "i"
generateUnLambdaFix (App f a) = liftA2 generateUnLambdaApp (generateUnLambdaFix f) (generateUnLambdaFix a)
generateUnLambdaFix other     = liftError $ "generateUnLambda " <> show other

generateUnLambdaApp :: (Semigroup a, IsString a) => a -> a -> a
generateUnLambdaApp f a = "`" <> f <> a

-- | Zot
generateZot :: MonadSafe m => Lambda -> m Text
generateZot l = appendError (show l) $ generateZotFix l

generateZotFix :: MonadSafe m => Lambda -> m Text
generateZotFix S         = pure "101010100"
generateZotFix K         = pure "1010100"
generateZotFix I         = pure "100"
generateZotFix (App f a) = liftA2 generateZotApp (generateZotFix f) (generateZotFix a)
generateZotFix other     = liftError $ "generateZot " <> show other

generateZotApp :: (Semigroup a, IsString a) => a -> a -> a
generateZotApp f a = "1" <> f <> a
