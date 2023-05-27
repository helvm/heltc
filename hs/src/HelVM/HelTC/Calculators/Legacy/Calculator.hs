module HelVM.HelTC.Calculators.Legacy.Calculator where

import           HelVM.HelTC.Calculators.Legacy.API.CalculusType

import           HelVM.HelTC.Calculators.Legacy.CodeGenerator
import           HelVM.HelTC.Calculators.Legacy.Parser
import           HelVM.HelTC.Calculators.Legacy.Reducer

import           HelVM.HelIO.Control.Safe

import           Control.Applicative.Tools

toCombinatorsText :: MonadSafe m =>  CalculusType -> CalculusType -> Text -> m Text
toCombinatorsText outputType inputType = generateCode outputType <=< reduce inputType outputType <.> parseLambdaText inputType
