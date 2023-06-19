module HelVM.HelTC.Calculators.Combinators.LazyK.Calculator (
  run,
  runWithTerminator,
  realize,
  realizeWithTrue,
) where

import           HelVM.HelTC.Calculators.Combinators.LazyK.Combinator
import           HelVM.HelTC.Calculators.Combinators.LazyK.Constants
import           HelVM.HelTC.Calculators.Combinators.LazyK.Reducer

import           HelVM.HelIO.Control.Safe

import           HelVM.HelIO.IO.BIO
import           HelVM.HelIO.IO.Console

-- | Calculator

run :: BIO m => Combinator -> m ()
run = runWithTerminator false

runWithTerminator :: BIO m => Combinator -> Combinator -> m ()
runWithTerminator terminator combinator = output terminator combinator =<< realizeWithTrue combinator

realizeWithTrue :: MonadSafe m => Combinator -> m Natural
realizeWithTrue = realize . flippedApply true

realize :: MonadSafe m => Combinator -> m Natural
realize = naturalSafe . flippedApply number0 . flippedApply Succ

number0 :: Combinator
number0 = Number 0

naturalSafe :: MonadSafe m => Combinator -> m Natural
naturalSafe (Number x) = pure x
naturalSafe         x  = liftErrorWithPrefix "Invalid output format. Output should be the list of Church numerals. " $ show x

output :: BIO m => Combinator -> Combinator -> Natural -> m ()
output terminator combinator number = check $ compare 256 number where
  check GT = wPutAsChar number *> runWithTerminator terminator (apply combinator terminator)
  check EQ = pass
  check LT = wLogStr (show number) *> wLogStr (show combinator)
