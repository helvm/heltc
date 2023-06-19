module HelVM.HelTC.Calculators.Lambda.Parser where

import           HelVM.HelTC.Calculators.Lambda.API.ILType
import           HelVM.HelTC.Calculators.Lambda.API.LambdaType

import qualified HelVM.HelTC.Calculators.Lambda.Parsers.MetaParser     as MetaParser
import qualified HelVM.HelTC.Calculators.Lambda.Parsers.SymbolicParser as SymbolicParser
import qualified HelVM.HelTC.Calculators.Lambda.Parsers.UnLambdaParser as UnLambdaParser
import qualified HelVM.HelTC.Calculators.Lambda.Parsers.ZotParser      as ZotParser

import           HelVM.HelTC.Calculators.Lambda.Lambda

import           HelVM.HelIO.IO.BIO
import           HelVM.HelIO.IO.FileReader

import           HelVM.HelIO.Control.Safe

import           Text.Trifecta

parseILFile :: BIO m => ILType -> FilePath -> m InstructionList
parseILFile ilType filePath = parseILText ilType =<< wReadFile filePath

parseILText :: MonadSafe m => ILType -> Text -> m InstructionList
parseILText ilType = fromResult . internalParser ilType . toString

parseLambdaText :: MonadSafe m => LambdaType -> Text -> m Lambda
parseLambdaText ilType = fromResult . internalParser2 ilType . toString

internalParser :: ILType -> String -> Result InstructionList
internalParser = flip parseString mempty . ilParser

internalParser2 :: LambdaType -> String -> Result Lambda
internalParser2 = flip parseString mempty . lambdaParser

fromResult :: MonadSafe m => Result a -> m a
fromResult (Success e) = pure e
fromResult (Failure e) = liftError $ show e

ilParser :: ILType -> Parser InstructionList
ilParser Meta     = MetaParser.ilParser
ilParser Symbolic = SymbolicParser.ilParser

lambdaParser :: LambdaType -> Parser Lambda
lambdaParser MLC      = MetaParser.lambdaParser
lambdaParser SLC      = SymbolicParser.lambdaParser
lambdaParser UnLambda = UnLambdaParser.lambdaParser
lambdaParser Zot      = ZotParser.lambdaParser
