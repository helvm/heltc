module HelVM.HelTC.Calculators.Combinators.DBLC.Parser where

import           HelVM.HelTC.Calculators.Combinators.DBLC.Term
import           HelVM.HelTC.Calculators.Combinators.DBLC.Token

-- Parse a string of 1s and 0s into a collection of terms

parse :: String -> [Token] -> [Term] -> [Term]
-- Tokenize
parse e@('1':_)       p  [] = let (i, s) = readInt e in parse s (PVar (i-1):p) []
parse ('0':'0':s)     p  [] = parse s (PApp:p) []
parse ('0':'1':'0':s) p  [] = parse s (PLam:p) []
parse ('0':'1':'1':s) p  [] = case readInt s of
  (0, s') -> parse s' (PU:p)           []
  (i, s') -> parse s' (PLevel (i-1):p) []
-- Build ASTs
parse [] (PLam:p)     (a:b:stk) = parse [] p (Lam a b:stk)
parse [] (PU:p)       stk       = parse [] p (U:stk)
parse [] (PApp:p)     (a:b:stk) = parse [] p ((a :% b):stk)
parse [] (PVar i:p)   stk       = parse [] p (Var i:stk)
parse [] (PLevel i:p) stk       = parse [] p (Level i:stk)
-- Finish
parse [] [] t = t
parse _ _ _ = error "parse"

readInt :: String -> (Int, String)
readInt ('1': s) = let (i, s') = readInt s in (i+1, s')
readInt ('0': s) = (0,s)
readInt other    = other & show & error

