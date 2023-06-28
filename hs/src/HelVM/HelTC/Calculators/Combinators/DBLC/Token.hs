module HelVM.HelTC.Calculators.Combinators.DBLC.Token where

data Token
  = PLam
  | PVar Int
  | PLevel Int
  | PApp
  | PU
