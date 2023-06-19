module HelVM.HelTC.Calculators.LC.Parsers.HaskellParser where

import           HelVM.HelTC.Calculators.LC.Lambda

import           HelVM.HelTC.Calculators.LC.Lexer

import           HelVM.HelIO.Extra                 hiding (Parser)

import           Text.Parser.Token
import           Text.Trifecta

parseUmlambda :: String -> Either String Lambda
parseUmlambda input = case parseString umlambdaExpr mempty input of
  Success e -> Right e
  Failure e -> Left (show e)

umlambdaExpr :: Parser Lambda
umlambdaExpr = do
  expr <- terminalParser
  _ <- optional newline
  pure expr

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
