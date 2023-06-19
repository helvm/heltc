module HelVM.HelTC.Calculators.Lambda.Generators.Impl.UnLambdaGenerator (
  generateCodeForLambda,
) where

import           HelVM.HelTC.Calculators.Lambda.Lambda

import           HelVM.HelIO.Control.Safe

generateCodeForLambda :: MonadSafe m => Lambda -> m Text
generateCodeForLambda l = appendError (show l) $ generateUnLambda l

generateUnLambda :: MonadSafe m => Lambda -> m Text
generateUnLambda S         = pure "s"
generateUnLambda K         = pure "k"
generateUnLambda I         = pure "i"
generateUnLambda (App f a) = liftA2 generateUnLambdaApp (generateUnLambda f) (generateUnLambda a)
generateUnLambda other     = liftError $ "generateUnLambda " <> show other

generateUnLambdaApp :: (Semigroup a, IsString a) => a -> a -> a
generateUnLambdaApp f a = "`" <> f <> a
