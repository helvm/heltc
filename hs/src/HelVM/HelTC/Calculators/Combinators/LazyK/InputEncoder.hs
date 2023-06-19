module HelVM.HelTC.Calculators.Combinators.LazyK.InputEncoder where

import           HelVM.HelTC.Calculators.Combinators.LazyK.Combinator
import           HelVM.HelTC.Calculators.Combinators.LazyK.Constants

import qualified Data.ByteString.Lazy                                 as LBS

-- | Constructors
readInput :: LBS.ByteString -> Combinator
readInput = encodeInput . fmap fromIntegral . LBS.unpack

encodeInput :: [Natural] -> Combinator
encodeInput = foldr (cons . church) end

-- | Other
end :: Combinator
end = cons (church 256) false

cons :: Combinator -> Combinator -> Combinator
cons a b = app3 S (app3SI (appK a)) (appK b)

church :: Natural -> Combinator
church 0   = false
church 1   = I
church 4   = appSelfApp $ church 2
church 8   = church 3 `App` church 2
church 9   = church 2 `App` church 3
church 16  = church 2 `App` church 4
church 25  = church 2 `App` church 5
church 27  = appSelfApp $ church 3
church 36  = church 2 `App` church 6
church 64  = church 3 `App` church 4
church 81  = church 4 `App` church 3
church 100 = church 2 `App` church 10
church 121 = church 2 `App` church 11
church 125 = church 3 `App` church 5
church 256 = appSelfApp $ church 4
church n   = succChurch $ n - 1

succChurch :: Natural -> Combinator
succChurch = successor . church

successor :: Combinator -> Combinator
successor = app3 S bCombinator
