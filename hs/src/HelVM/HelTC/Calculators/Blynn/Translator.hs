module HelVM.HelTC.Calculators.Blynn.Translator where

import           HelVM.HelTC.Calculators.Blynn.Lambda

import qualified HelVM.HelTC.Calculators.LC.Lambda    as Hel

translate :: Hel.Lambda -> LC
translate Hel.S = Other S
translate Hel.K = Other K
translate Hel.I = Other I
translate Hel.Var n = 

