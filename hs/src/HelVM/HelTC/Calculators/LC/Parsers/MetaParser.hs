module HelVM.HelTC.Calculators.LC.Parsers.MetaParser (
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
rootLambdaLnParser = rootLambdaParser <* commentParser <?> "rootln"

rootLambdaParser :: Parser Instruction
rootLambdaParser = defineParser <|> evalParser <?> "root"

defineParser :: Parser Instruction
defineParser = liftA2 Def defineNameParser lambdaParser <?> "define"

defineNameParser :: Parser Identifier
defineNameParser = (colon *> identifierParser) <?> "defineName"

evalParser :: Parser Instruction
evalParser =  Eval <$> lambdaParser <?> "evalParser"

-- | Lambda Parsers
lambdaParser :: Parser Lambda
lambdaParser = abstractionParser <|> applicationParser

abstractionParser :: Parser Lambda
abstractionParser = liftA2 Abs paramParser lambdaParser <?> "abstraction"

paramParser :: Parser Identifier
paramParser = (backslash *> identifierOrUnderscoreParser) <?> "param"

applicationParser :: Parser Lambda
applicationParser = lambdaFromNonEmpty <$> many1' terminalParser

terminalParser :: Parser Lambda
terminalParser =
      parens lambdaParser
  <|> listParser
  <|> stringParser
  <|> charParser
  <|> signedParser
  <|> naturalParser
  <|> variableParser

listParser :: Parser Lambda
listParser = List <$> brackets (many lambdaParser)

stringParser :: Parser Lambda
stringParser = Str <$> stringLiteral <?> "string"

charParser :: Parser Lambda
charParser = lambdaFromChar <$> charLiteral' <?> "char"

signedParser :: Parser Lambda
signedParser = Int <$> liftA2 SN signParser naturalLiteral <?> "integer"

naturalParser :: Parser Lambda
naturalParser = lambdaFromIntegral <$> naturalLiteral <?> "natural"

variableParser :: Parser Lambda
variableParser = lambdaFromVariable <$> identifierParser <?> "variable"
