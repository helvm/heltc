module HelVM.HelTC.Calculators.LC.Lexer where

import           HelVM.HelTC.Calculator.Value

import           Control.Type.Operator

import           Data.Char

import           Text.Parser.Token
import           Text.Trifecta

identifierOrUnderscoreParser :: Parser Identifier
identifierOrUnderscoreParser = identifierParser <|> underscore

identifierParser :: Parser Identifier
identifierParser = specialIdentifierParser <|> alphaNumIdentifierParser

specialIdentifierParser :: Parser Identifier
specialIdentifierParser = toIdentifier <$> token (some specialChar) <?> "specialIdentifierParser"

specialChar :: Parser Char
specialChar = satisfy isSpecial <?> "specialChar"

isSpecial :: Char -> Bool
isSpecial = flip elem ("@,.&|<>=+-*/~" :: String)

alphaNumIdentifierParser :: Parser Identifier
alphaNumIdentifierParser = toIdentifier <$> token (liftA2 (:) letterChar $ many alphaNumChar)

charLiteral' :: Parser Char
charLiteral' = token (char '\'' *> characterChar) <?> "charLiteral'"

signParser :: Parser Sign
signParser = (Plus <$ plus) <|> (Minus <$ minus)

naturalLiteral :: Parser Natural
naturalLiteral = fromIntegral <$> token decimal

commentParser :: Parser $ Maybe String
commentParser = token $ optional $ many $ token $ char ';' <* many (noneOf "\n")

newlineParser :: Parser Char
newlineParser = token newline

backslash , backtick :: Parser Char
backslash  = symbolic '\\'
backtick   = symbolic '`'

minus , plus :: Parser Char
minus = symbolic '-'
plus  = symbolic '+'

underscore :: Parser Text
underscore  = one <$> symbolic '_'

defineSymbol :: Parser Text
defineSymbol = textSymbol "define"

lambdaSymbol :: Parser Text
lambdaSymbol = textSymbol "lambda"

letterChar :: Parser Char
letterChar = satisfy isLetter <?> "letter"

alphaNumChar :: Parser Char
alphaNumChar = satisfy isAlphaNum <?> "alphanumeric character"
