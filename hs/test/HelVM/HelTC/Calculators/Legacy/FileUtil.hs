module HelVM.HelTC.Calculators.Legacy.FileUtil (
  readCalculusFile,
  buildAbsoluteCalculusExtFileName,
  buildAbsolutePathToIlFile,

  dir,
  showAscii,
  options,
) where

import           HelVM.HelIO.Extra

import           System.FilePath.Posix

readCalculusFile :: FilePath -> FilePath -> IO Text
readCalculusFile lang fileName = readFileTextUtf8 ("examples" </> dir </> lang </> fileName <.> lang)

buildAbsoluteCalculusExtFileName :: FilePath -> FilePath -> FilePath -> FilePath
buildAbsoluteCalculusExtFileName = buildAbsoluteExtFileName

buildAbsolutePathToIlFile :: FilePath -> FilePath -> FilePath
buildAbsolutePathToIlFile lang fileName = dir </> lang </> "il" </> fileName <.> "il"

----

buildAbsoluteExtFileName :: FilePath -> FilePath -> FilePath -> FilePath
buildAbsoluteExtFileName lang ext fileName = dir </> lang </> ext </> fileName <.> ext

dir :: FilePath
dir = "legacy"

showAscii:: Bool -> FilePath
showAscii False = "asciiOff"
showAscii True  = "asciiOn"

options :: [Bool]
options = [True , False]

