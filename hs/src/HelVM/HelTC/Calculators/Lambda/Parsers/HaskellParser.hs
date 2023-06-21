module HelVM.HelTC.Calculators.Lambda.Parsers.HaskellParser where

import           HelVM.HelTC.Calculators.Lambda.Lambda

import           HelVM.HelTC.Calculators.Lambda.Lexer

import           HelVM.HelIO.Extra                     hiding (Parser)

import           Text.Parser.Token
import           Text.Trifecta

parseUmLambda :: String -> Either String Lambda
parseUmLambda = parseString umlambdaExpr mempty <&> check

check :: Result a -> Either String a
check = \ case
  Success e -> Right e
  Failure e -> show e & Left

umlambdaExpr :: Parser Lambda
umlambdaExpr = terminalParser <* optional newline

terminalParser :: Parser Lambda
terminalParser = (absParser <|> applicationParser <|> variableParser) <?> "terminal"

absParser :: Parser Lambda
absParser = do
  _ <- symbol "\\"
  var <- identifierParser
  _ <- symbol "->"
  Abs var <$> terminalParser

applicationParser :: Parser Lambda
applicationParser = (lambdaFromNonEmpty <$> parens (many1' terminalParser)) <?> "application"

variableParser :: Parser Lambda
variableParser = (Var <$> identifierParser) <?> "variable"
