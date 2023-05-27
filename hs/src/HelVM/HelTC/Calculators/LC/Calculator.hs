module HelVM.HelTC.Calculators.LC.Calculator where

import           HelVM.HelTC.Calculators.LC.API.ILType
import           HelVM.HelTC.Calculators.LC.API.LambdaType

import           HelVM.HelTC.Calculators.LC.Generators.Generator

import           HelVM.HelTC.Calculators.LC.DefinitionExpander
import           HelVM.HelTC.Calculators.LC.Lambda
import           HelVM.HelTC.Calculators.LC.LambdaReducer
import           HelVM.HelTC.Calculators.LC.Parser

import           HelVM.HelTC.Calculator.IO.BusinessIO

import           HelVM.HelIO.Control.Safe

import           Control.Applicative.Tools

import           HelVM.HelTC.Calculators.LC.Reducers.SkiReducer

translateFile :: BIO m => ILType -> ILType -> FilePath -> m Text
translateFile generatorType parserType filePath = translateText generatorType parserType =<< wReadFile filePath

minimizeText :: BIO m => ILType -> Text -> m Text
minimizeText ilType = translateText ilType ilType

translateText :: MonadSafe m => ILType -> ILType -> Text -> m Text
translateText generatorType parserType = generateCodeForIL generatorType <.> parseILText parserType

toCombinatorsFile :: BIO m => LambdaType -> ILType -> FilePath -> m Text
toCombinatorsFile generatorType parserType = toCombinatorsText generatorType parserType <=< wReadFile

toCombinatorsText :: MonadSafe m => LambdaType -> ILType -> Text -> m Text
toCombinatorsText generatorType parserType = generateCodeForLambda generatorType <=< reduceText parserType

reduceText :: MonadSafe m => ILType -> Text -> m Lambda
reduceText ilType = reduceIL <=< parseILText ilType

reduceIL :: MonadSafe m => InstructionList -> m Lambda
reduceIL = extract <=< expandDefinitions . reduceLambda

extract :: MonadSafe m => LambdaList -> m Lambda
extract []  = pure I
extract [l] = pure l
extract ll  = liftErrorWithPrefix "more then one" $ show ll

toAbstractionFile :: BIO m => ILType -> LambdaType -> FilePath -> m Text
toAbstractionFile generatorType parserType = toAbstractionText generatorType parserType <=< wReadFile

toAbstractionText :: MonadSafe m => ILType -> LambdaType -> Text -> m Text
toAbstractionText generatorType parserType = generateCodeForAbstract generatorType <.> reduceSki <.> parseLambdaText parserType
