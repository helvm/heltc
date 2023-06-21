module HelVM.HelTC.Calculators.Combinators.Zot.FileExtra (
  readZotFile,
  buildAbsoluteZotFileName,
  buildAbsoluteZotOutFileName,
  buildAbsoluteZotLogFileName,
  binaryOnly,
  options,
) where

import           HelVM.HelTC.Calculators.FileExtra

import           HelVM.HelTC.Calculator.API.IOTypes

import           HelVM.HelTC.Calculator.Types.FormatType

readZotFile :: FilePath -> IO Source
readZotFile = readSourceFile . buildAbsoluteZotFileName

buildAbsoluteZotFileName :: FilePath -> FilePath
buildAbsoluteZotFileName = buildAbsoluteLangFileName lang

buildAbsoluteZotOutFileName :: FilePath -> FilePath
buildAbsoluteZotOutFileName = buildAbsoluteOutFileName lang

buildAbsoluteZotLogFileName :: FilePath -> FilePath
buildAbsoluteZotLogFileName = buildAbsoluteLogFileName lang

lang :: FilePath
lang = "zot"

binaryOnly :: [FormatType]
binaryOnly = [BinaryLabel]
