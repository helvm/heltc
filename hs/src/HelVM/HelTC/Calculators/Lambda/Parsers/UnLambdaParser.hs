module HelVM.HelTC.Calculators.Lambda.Parsers.UnLambdaParser (
  ilParser,
  lambdaParser,
  parseUnLambda,
) where

import           HelVM.HelTC.Calculators.Lambda.Lambda

import           HelVM.HelTC.Calculators.Lambda.Lexer

import           Text.Parser.Token
import           Text.Trifecta

ilParser :: Parser InstructionList
ilParser = one <$> instructionParser

instructionParser :: Parser Instruction
instructionParser = Eval <$> lambdaParser

lambdaParser :: Parser Lambda
lambdaParser = applicationParser <|> sParser <|> kParser <|> iParser

applicationParser :: Parser Lambda
applicationParser = backtick *> liftA2 App lambdaParser lambdaParser

sParser , kParser , iParser :: Parser Lambda
sParser = S <$ symbolic 's'
kParser = K <$ symbolic 'k'
iParser = I <$ symbolic 'i'


--FIXME TO REMOVE

parseUnLambda :: Text -> Lambda
parseUnLambda = fst . unLambdaParser . toString

unLambdaParser :: String -> (Lambda , String)
unLambdaParser ('`' : s) = graveParser s
unLambdaParser (c   : s) = (charParser c , s)
unLambdaParser        s  = error $ "unLambdaParser error" <> toText s

graveParser :: String -> (Lambda , String)
graveParser s = (App f a , s'') where
  (a, s'') = unLambdaParser s'
  (f, s')  = unLambdaParser s

charParser :: Char -> Lambda
charParser 's' = S
charParser 'k' = K
charParser 'i' = I
charParser  c  = error $ "charParser: " <> one c
