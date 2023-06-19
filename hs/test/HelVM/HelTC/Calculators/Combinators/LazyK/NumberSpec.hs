module HelVM.HelTC.Calculators.Combinators.LazyK.NumberSpec (spec) where

import           HelVM.HelTC.Calculators.Combinators.LazyK.InputEncoder

import           HelVM.HelTC.Calculators.Combinators.LazyK.Calculator
import           HelVM.HelTC.Calculators.Combinators.LazyK.Reducer

import           HelVM.HelIO.Control.Safe

import           HelVM.Expectations

import           Test.Hspec                                             (Spec, describe, it)

spec :: Spec
spec =
  describe "special church" $ forM_
    [0, 1, 4, 8, 9, 16, 27, 36, 64, 81, 100, 121, 125, 256] $ \number ->
    it ("special church " <> show number) $
       f number `shouldSafe` number

f :: MonadSafe m => Natural -> m Natural
f = realize . reduce . church
