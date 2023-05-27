module HelVM.HelTC.Calculators.Legacy.Parsers.LambdaParser where

import           HelVM.HelTC.Calculators.LC.Lambda

import           HelVM.HelTC.Calculators.Legacy.Parse

import           Data.Char

import qualified Relude.Unsafe                        as Unsafe

parseLambda :: Text -> Lambda
parseLambda = fst . fst . Unsafe.head . (lambdaParser >*> eof) . lexer . toString

lambdaParser :: Parse String Lambda
lambdaParser = applyParser `alt` funParser

applyParser :: Parse String Lambda
applyParser = recL1 App atomParser

funParser :: Parse String Lambda
funParser = token "\\" >*> paramsParser >*> token "->" >*> lambdaParser `build` buildFun

buildFun :: (String, ([String], (String, Lambda))) -> Lambda
buildFun ("\\", (ps, ("->", e))) = makeFun (toText <$> ps) e
buildFun  _                      = error "buildFun"

makeFun :: [Text] -> Lambda -> Lambda
makeFun [p] ex      = Abs p ex
makeFun (p : ps) ex = Abs p $ makeFun ps ex
makeFun _ _         = error "makeFun error: need 1 parameter at least"

atomParser :: Parse String Lambda
atomParser = (spot (isLower . Unsafe.head) `build` (Var . toText)) `alt` parensParser

parensParser :: Parse String Lambda
parensParser = token "(" >*> lambdaParser >*> token ")" `build` fst . snd

paramsParser :: Parse String [String]
paramsParser = list1 $ spot $ isLower . Unsafe.head

lexer :: String -> [String]
lexer             ""  = []
lexer (' '       : s) = lexer s
lexer ('\n'      : s) = lexer s
lexer ('\\'      : s) = "\\" : lexer s
lexer ('-' : '>' : s) = "->" : lexer s
lexer (      '(' : s) = "("  : lexer s
lexer (      ')' : s) = ")"  : lexer s
lexer ca@(c : _)
  | isLower c    = ret : lexer s where (ret, s) = span isAlphaNum ca
lexer ca      = error $ "lexer error: " <> toText ca
