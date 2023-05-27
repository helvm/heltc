module HelVM.HelTC.Calculators.LC.Parsers.ZotParser (
  ilParser,
  lambdaParser,
) where

import           HelVM.HelTC.Calculators.LC.Lambda

--import           HelVM.HelTC.Calculators.LC.Lexer

import           Text.Parser.Token
import           Text.Trifecta

ilParser :: Parser InstructionList
ilParser = one <$> instructionParser

instructionParser :: Parser Instruction
instructionParser = Eval <$> lambdaParser

lambdaParser :: Parser Lambda
lambdaParser = applicationParser <|> sParser <|> kParser <|> iParser

applicationParser :: Parser Lambda
applicationParser = symbolic '1' *> (liftA2 App lambdaParser lambdaParser)

sParser , kParser , iParser :: Parser Lambda
sParser = S <$ textSymbol "101010100"
kParser = K <$ textSymbol "1010100"
iParser = I <$ textSymbol "100"
