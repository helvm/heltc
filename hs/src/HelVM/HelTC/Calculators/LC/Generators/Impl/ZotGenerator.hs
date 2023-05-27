module HelVM.HelTC.Calculators.LC.Generators.Impl.ZotGenerator (
  generateCodeForLambda,
) where

import           HelVM.HelTC.Calculators.LC.Lambda

import           HelVM.HelIO.Control.Safe

generateCodeForLambda :: MonadSafe m => Lambda -> m Text
generateCodeForLambda l = appendError (show l) $ generateZot l

generateZot :: MonadSafe m => Lambda -> m Text
generateZot S         = pure "101010100"
generateZot K         = pure "1010100"
generateZot I         = pure "100"
generateZot (App f a) = liftA2 generateZotApp (generateZot f) (generateZot a)
generateZot other     = liftError $ "generateZot " <> show other

generateZotApp :: (Semigroup a, IsString a) => a -> a -> a
generateZotApp f a = "1" <> f <> a
