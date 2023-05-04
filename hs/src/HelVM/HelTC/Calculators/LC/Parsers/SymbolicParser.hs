module HelVM.HelTC.Calculators.LC.Parsers.SymbolicParser (
  ilParser,
) where

import           HelVM.HelTC.Calculators.LC.Lambda
import           HelVM.HelTC.Calculators.LC.Lexer

import           HelVM.HelTC.Calculator.Value

import           HelVM.HelIO.Extra                 hiding (Parser)

import           Text.Parser.Token
import           Text.Trifecta

ilParser :: Parser InstructionList
ilParser = commentParser *> many rootLambdaLnParser <* eof

-- | Instruction Parsers
rootLambdaLnParser :: Parser Instruction
rootLambdaLnParser = rootLambdaParser <?> "rootln"

rootLambdaParser :: Parser Instruction
rootLambdaParser = parens (defineParser <|> evalParser) <?> "root"

defineParser :: Parser Instruction
defineParser = liftA2 Def (defineSymbol *> identifierParser) lambdaParser <?> "define"

evalParser :: Parser Instruction
evalParser = (Eval <$> lambdaParser) <?> "eval"

-- | Lambda Parsers
lambdaParser :: Parser Lambda
lambdaParser = (abstractionParser <|> applicationParser) <?> "lambda"

abstractionParser :: Parser Lambda
abstractionParser = liftA2 AbsRe paramsParser lambdaParser <?> "abstractionRe"

paramsParser :: Parser IdentifierList
paramsParser = (lambdaSymbol *> parens (some identifierOrUnderscoreParser)) <?> "params"

applicationParser :: Parser Lambda
applicationParser = (lambdaFromNonEmpty <$> many1' terminalParser) <?> "application"

terminalParser :: Parser Lambda
terminalParser = (
      parens lambdaParser
  <|> listParser
  <|> stringParser
  <|> charParser
  <|> signedParser
  <|> naturalParser
  <|> variableParser
  ) <?> "terminal"

listParser :: Parser Lambda
listParser = (List <$> brackets (many lambdaParser)) <?> "list"

stringParser :: Parser Lambda
stringParser = (Str <$> stringLiteral) <?> "string"

charParser :: Parser Lambda
charParser = (lambdaFromChar <$> charLiteral') <?> "char"

signedParser :: Parser Lambda
signedParser = (Int <$> liftA2 SN signParser naturalLiteral) <?> "integer"

naturalParser :: Parser Lambda
naturalParser = (lambdaFromIntegral <$> naturalLiteral) <?> "natural"

variableParser :: Parser Lambda
variableParser = (lambdaFromVariable <$> identifierParser) <?> "variable"
