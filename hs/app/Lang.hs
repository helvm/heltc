module Lang where

import           HelVM.HelIO.SwitchEnum

defaultLang :: Lang
defaultLang = defaultEnum

langs :: [Lang]
langs = generateEnums 1

data Lang = MLC
  deriving stock (Bounded , Enum , Eq , Read , Show)
