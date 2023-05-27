module HelVM.HelTC.Calculators.Legacy.API.AssemblyOptions where

import           HelVM.HelTC.Calculators.Legacy.API.Separator

import           HelVM.HelTC.Calculators.Legacy.API.CalculusType

data AssemblyOptions = AssemblyOptions
  { separator  :: Maybe Separator
  , inputType  :: CalculusType
  , outputType :: CalculusType
  }
