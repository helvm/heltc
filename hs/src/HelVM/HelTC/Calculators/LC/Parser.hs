module HelVM.HelTC.Calculators.LC.Parser where

import           HelVM.HelTC.Calculators.LC.API.ParserType

import qualified HelVM.HelTC.Calculators.LC.Parsers.MetaParser     as MetaParser
import qualified HelVM.HelTC.Calculators.LC.Parsers.SymbolicParser as SymbolicParser

import           HelVM.HelTC.Calculators.LC.Lambda

import           HelVM.HelTC.Calculator.IO.BusinessIO

import           HelVM.HelIO.Control.Safe

import           Text.Trifecta

parseAssemblyFile :: BIO m => ParserType -> FilePath -> m InstructionList
parseAssemblyFile parserType filePath = parseAssemblyText parserType =<< wReadFile filePath

parseAssemblyText :: MonadSafe m => ParserType -> Text -> m InstructionList
parseAssemblyText parserType = fromResult . internalParser parserType . toString

internalParser :: ParserType -> String -> Result InstructionList
internalParser = flip parseString mempty . ilParser

fromResult :: MonadSafe m => Result a -> m a
fromResult (Success e) = pure e
fromResult (Failure e) = liftError $ show e

ilParser :: ParserType -> Parser InstructionList
ilParser Meta     = MetaParser.ilParser
ilParser Symbolic = SymbolicParser.ilParser
