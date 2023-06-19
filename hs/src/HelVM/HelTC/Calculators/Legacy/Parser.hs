module HelVM.HelTC.Calculators.Legacy.Parser (
  parseLambdaText,
) where

import           HelVM.HelTC.Calculators.Legacy.API.CalculusType

import           HelVM.HelTC.Calculators.Lambda.Lambda

import           HelVM.HelTC.Calculators.Lambda.Parsers.UnLambdaParser
import           HelVM.HelTC.Calculators.Legacy.Parsers.LambdaParser
import           HelVM.HelTC.Calculators.Legacy.Parsers.ZotParser

import           HelVM.HelIO.Control.Safe

parseLambdaText :: MonadSafe m =>  CalculusType -> Text -> m Lambda
parseLambdaText Zot        = pure . parseZot
parseLambdaText UnLambda   = pure . parseUnLambda
parseLambdaText Combinator = pure . parseLambda
parseLambdaText Lambda     = pure . parseLambda
