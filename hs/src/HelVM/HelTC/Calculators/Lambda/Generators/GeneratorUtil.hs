module HelVM.HelTC.Calculators.Lambda.Generators.GeneratorUtil where

import           HelVM.HelTC.Calculators.Lambda.Lambda

import           HelVM.HelTC.Calculator.Value

par :: (Lambda -> Text) -> Lambda -> Text
par sh a@(App _ _) = "(" <> sh a <> ")"
par sh f@(Abs _ _) = "(" <> sh f <> ")"
par sh e           = sh e

showSign :: Sign -> Text
showSign Plus  = "+"
showSign Minus = "-"
