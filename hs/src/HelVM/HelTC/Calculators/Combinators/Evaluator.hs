module HelVM.HelTC.Calculators.Combinators.Evaluator where

import           HelVM.HelTC.Calculators.Combinators.API.CombinatorsType

import           HelVM.HelTC.Calculator.Types.FormatType

import qualified HelVM.HelTC.Calculators.Combinators.LazyK.Evaluator     as LazyK
import qualified HelVM.HelTC.Calculators.Combinators.Zot.Calculator      as Zot

import           HelVM.HelTC.Calculator.API.IOTypes

import           HelVM.HelIO.IO.BIO
import           HelVM.HelIO.IO.Console


evalSource :: BIO m => CombinatorsType -> Source -> m ()
evalSource = \ case
  LazyK -> LazyK.evalSource
  Zot   -> Zot.evalSource TextLabel >=> wPutStr
