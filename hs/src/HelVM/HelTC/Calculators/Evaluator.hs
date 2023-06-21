module HelVM.HelTC.Calculators.Evaluator where

import           HelVM.HelTC.Calculators.API.CalculatorParams

import qualified HelVM.HelTC.Calculators.Combinators.Evaluator as Combinators
import qualified HelVM.HelTC.Calculators.Lambda.Calculator     as Lambda

import           HelVM.HelIO.IO.BIO
import           HelVM.HelIO.IO.Console

evalSource :: BIO m => CalculatorParams -> Text -> m ()
evalSource = \ case
  (Lambda lambdaType ilType)    -> Lambda.toCombinatorsText lambdaType ilType >=> wPutStr
  (Combinators combinatorsType) -> Combinators.evalSource combinatorsType
