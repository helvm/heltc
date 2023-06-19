module HelVM.HelTC.Calculators.Blynn.Translator where

import           HelVM.HelTC.Calculators.Blynn.Lambda

import qualified HelVM.HelTC.Calculators.LC.Lambda    as Hel

translate :: Hel.Lambda -> LC
translate  Hel.S        = Other S
translate  Hel.K        = Other K
translate  Hel.I        = Other I
translate (Hel.Var n)   = Var $ toString n
translate (Hel.Abs n f) = Lam (toString n) (translate f)
translate (Hel.App f g) = translate f :@ translate g
translate _             = error "error in translate"
