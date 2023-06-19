module HelVM.HelTC.Calculators.Combinators.LazyK.Parser (
  parse,
) where

import           HelVM.HelTC.Calculators.Combinators.LazyK.Combinator
import           HelVM.HelTC.Calculators.Combinators.LazyK.Lexer

import           HelVM.HelTC.Calculator.API.IOTypes
import           HelVM.HelTC.Calculator.ReadPExtra

import           HelVM.HelIO.Control.Safe

import           Text.ParserCombinators.ReadP                         hiding (many)

parse :: MonadSafe m => Source -> m Combinator
parse = parseCode . filterComments

parseCode :: MonadSafe m => Source -> m Combinator
parseCode = runParser appParser

appParser :: ReadP Combinator
appParser = foldlCombinator <$> manyNonEmpty combinatorParser

combinatorParser :: ReadP Combinator
combinatorParser =
       S <$ oneOf "sS"
  <|>  K <$ oneOf "kK"
  <|>  I <$ oneOf "iI"
  <|>  App <$ char '`' <*> combinatorParser <*> combinatorParser
  <|>  char '(' *> appParser <* char ')'
