module HelVM.HelTC.Calculators.Blynn.Translator where

import qualified HelVM.HelTC.Calculators.Blynn.Lambda as Blynn

import qualified HelVM.HelTC.Calculators.LC.Lambda    as Hel

translate :: Hel.Lambda -> Blynn.LC
