module HelVM.HelTC.Calculators.LC.FileUtil (
  readLangFile,
  readSourceFile,
  buildAbsolutePathToSourceFile,
  buildAbsoluteExpandedFileName,
  buildAbsoluteReducedFileName,
  buildAbsoluteParsedFileName,
  buildAbsoluteIlFileName,
  buildAbsoluteLangFileName,
  buildAbsoluteModeFileName,
  buildAbsoluteExtFileName,
  buildAbsoluteOutFileName,
  buildAbsoluteLogFileName,
  buildAbsoluteEvalFileName,
  showAscii,
  options,
) where

import           HelVM.HelIO.Extra

import           System.FilePath.Posix

readLangFile :: MonadIO m => FilePath -> FilePath -> m Text
readLangFile lang fileName = readSourceFile $ lang </> fileName <.> lang

readSourceFile :: MonadIO m => FilePath -> m Text
readSourceFile filePath = readFileTextUtf8 $ familyDir </> filePath

buildAbsolutePathToSourceFile :: FilePath -> FilePath -> FilePath
buildAbsolutePathToSourceFile lang fileName = familyDir </> lang </> fileName <.> lang

buildAbsoluteExpandedFileName :: FilePath -> FilePath -> FilePath
buildAbsoluteExpandedFileName = buildAbsoluteExtFileName "expanded"

buildAbsoluteReducedFileName :: FilePath -> FilePath -> FilePath
buildAbsoluteReducedFileName = buildAbsoluteExtFileName "reduced"

buildAbsoluteParsedFileName :: FilePath -> FilePath -> FilePath
buildAbsoluteParsedFileName = buildAbsoluteExtFileName "parsed"

buildAbsoluteIlFileName :: FilePath -> FilePath -> FilePath
buildAbsoluteIlFileName = buildAbsoluteExtFileName "il"

buildAbsoluteLangFileName :: FilePath -> FilePath -> FilePath
buildAbsoluteLangFileName lang fileName = family </> lang </> fileName <.> lang

buildAbsoluteModeFileName :: FilePath -> FilePath -> FilePath -> FilePath
buildAbsoluteModeFileName mode lang fileName = family </> lang </> mode </> fileName <.> lang

buildAbsoluteExtFileName :: FilePath -> FilePath -> FilePath -> FilePath
buildAbsoluteExtFileName ext lang fileName = family </> lang </> ext </> fileName <.> ext

buildAbsoluteOutFileName :: FilePath -> FilePath -> FilePath
buildAbsoluteOutFileName = buildAbsoluteEvalFileName "output"

buildAbsoluteLogFileName :: FilePath -> FilePath -> FilePath
buildAbsoluteLogFileName = buildAbsoluteEvalFileName "logged"

buildAbsoluteEvalFileName :: FilePath -> FilePath -> FilePath -> FilePath
buildAbsoluteEvalFileName mode lang fileName = family </> lang </> "eval" </> mode </> fileName <.> mode

showAscii:: Bool -> FilePath
showAscii False = "asciiOff"
showAscii True  = "asciiOn"

options :: [Bool]
options = [True , False]

familyDir :: FilePath
familyDir = dir </> family

dir :: FilePath
dir = "examples"

family :: FilePath
family = "lc"
