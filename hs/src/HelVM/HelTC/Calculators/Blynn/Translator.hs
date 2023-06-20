module HelVM.HelTC.Calculators.Blynn.Translator where

import           HelVM.HelTC.Calculators.Blynn.Lambda

import qualified HelVM.HelTC.Calculators.LC.Lambda    as Hel

translateToCL :: Hel.Lambda -> CL
translateToCL  Hel.S        = Lf S
translateToCL  Hel.K        = Lf K
translateToCL  Hel.I        = Lf I
translateToCL (Hel.App f g) = translateToCL f :# translateToCL g
translateToCL _             = error "error in translateToCL"


translateToLC :: Hel.Lambda -> LC
translateToLC  Hel.S        = Other S
translateToLC  Hel.K        = Other K
translateToLC  Hel.I        = Other I
translateToLC (Hel.App f g) = translateToLC f :@ translateToLC g
translateToLC (Hel.Var n)   = Var $ toString n
translateToLC (Hel.Abs n f) = Lam (toString n) (translateToLC f)
translateToLC _             = error "error in translateToLC"
