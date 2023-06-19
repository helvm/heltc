module HelVM.HelTC.Calculators.Combinators.LazyK.Evaluator (
  evalSource,
  evalLambda,
  evalCombinator,
  reduceSource,
) where

import           HelVM.HelTC.Calculators.Combinators.LazyK.Calculator
import           HelVM.HelTC.Calculators.Combinators.LazyK.Combinator
import           HelVM.HelTC.Calculators.Combinators.LazyK.InputEncoder
import           HelVM.HelTC.Calculators.Combinators.LazyK.Parser

import           HelVM.HelTC.Calculators.Combinators.LazyK.Reducer

import qualified HelVM.HelTC.Calculators.Lambda.Lambda                  as L

import           HelVM.HelTC.Calculator.API.IOTypes

import           HelVM.HelIO.IO.BIO
import           HelVM.HelIO.IO.Console

evalSource :: BIO m => Source -> m ()
evalSource = evalCombinator <=< parse

evalLambda :: BIO m => L.Lambda -> m ()
evalLambda = evalCombinator . fromLambda

evalCombinator :: BIO m => Combinator -> m ()
evalCombinator combinator = (run . reduce . App combinator . readInput) =<< wGetContentsBS

reduceSource :: BIO m => Source -> m Source
reduceSource s = show . reduce <$> parse s
