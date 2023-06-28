module HelVM.HelTC.Calculators.Combinators.DBLC.FileExtra (
  readDBLCFile,
  buildAbsoluteMinifiedDBLCFileName,
  buildAbsoluteDBLCFileName,
  buildAbsoluteDBLCLambdaFileName,
  buildAbsoluteDBLCOutFileName,
  buildAbsoluteDBLCLogFileName,
  showCompile,
  options,
) where

import           HelVM.HelTC.Calculators.FileExtra

import           HelVM.HelTC.Calculator.API.IOTypes

readDBLCFile :: FilePath -> IO Source
readDBLCFile = readSourceFile . buildAbsoluteDBLCFileName

buildAbsoluteMinifiedDBLCFileName :: FilePath -> FilePath
buildAbsoluteMinifiedDBLCFileName = buildAbsoluteModeFileName "minified" lang

buildAbsoluteDBLCFileName :: FilePath -> FilePath
buildAbsoluteDBLCFileName = buildAbsoluteLangFileName lang

buildAbsoluteDBLCLambdaFileName :: FilePath -> FilePath
buildAbsoluteDBLCLambdaFileName = buildAbsoluteLambdaFileName lang

buildAbsoluteDBLCOutFileName :: FilePath -> FilePath
buildAbsoluteDBLCOutFileName = buildAbsoluteOutFileName lang

buildAbsoluteDBLCLogFileName :: FilePath -> FilePath
buildAbsoluteDBLCLogFileName = buildAbsoluteLogFileName lang

lang :: FilePath
lang = "dblc"

showCompile :: Bool -> FilePath
showCompile False = "token"
showCompile True  = "instruction"
