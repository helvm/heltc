module HelVM.HelTC.Calculators.LC.Calculator where

import           HelVM.HelTC.Calculators.LC.API.GeneratorType
import           HelVM.HelTC.Calculators.LC.API.ParserType

import           HelVM.HelTC.Calculators.LC.Generators.ExpressionGenerator
import           HelVM.HelTC.Calculators.LC.Generators.LambdaGenerator

import           HelVM.HelTC.Calculators.LC.DefinitionExpander
import           HelVM.HelTC.Calculators.LC.Lambda
import           HelVM.HelTC.Calculators.LC.LambdaReducer
import           HelVM.HelTC.Calculators.LC.Parser

import           HelVM.HelTC.Calculator.IO.BusinessIO

import           HelVM.HelIO.Control.Safe

translateFile :: BIO m => ParserType -> ParserType -> FilePath -> m Text
translateFile generatorType parserType filePath = translateText generatorType parserType =<< wReadFile filePath

minimizeText :: BIO m => ParserType -> Text -> m Text
minimizeText parserType = translateText parserType parserType

translateText :: MonadSafe m => ParserType -> ParserType -> Text -> m Text
translateText generatorType parserType source = generateCodeForIL generatorType <$> parseAssemblyText parserType source

assembleFile :: BIO m => GeneratorType -> ParserType -> FilePath -> m Text
assembleFile generatorType parserType filePath = assembleText generatorType parserType =<< wReadFile filePath

assembleText :: MonadSafe m => GeneratorType -> ParserType -> Text -> m Text
assembleText generatorType parserType source = generateCodeForLambda generatorType =<< reduceText parserType source

reduceText :: MonadSafe m => ParserType -> Text -> m Lambda
reduceText parserType source = reduceIL =<< parseAssemblyText parserType source

reduceIL :: MonadSafe m => InstructionList -> m Lambda
reduceIL = extract <=< expandDefinitions . reduceLambda

extract :: MonadSafe m => LambdaList -> m Lambda
extract []  = pure I
extract [l] = pure l
extract ll  = liftErrorWithPrefix "more then one" $ show ll
