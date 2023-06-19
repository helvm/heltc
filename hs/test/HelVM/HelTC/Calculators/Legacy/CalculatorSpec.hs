module HelVM.HelTC.Calculators.Legacy.CalculatorSpec where

import           HelVM.HelTC.Calculators.Legacy.API.CalculusType
import           HelVM.HelTC.Calculators.Legacy.Calculator
import           HelVM.HelTC.Calculators.Legacy.FileUtil

import           HelVM.HelIO.Control.Safe
import           HelVM.HelIO.ZipA

import           HelVM.GoldenExpectations

import           Data.Char

import           System.FilePath.Posix

import           Test.Hspec                                      (Spec, describe, it)

spec :: Spec
spec = describe "calculus" $ forM_ ((
    [ "swap"
    , "reverse"
    ] |><| ["barker"])
    <> (
    [ "flipPrint"
    , "id"
    , "print3"
    , "rev"
    , "reverse"
--    , "hello"
    ] |><| ["YoshikuniJujo"]
    )) $ \(fileName , dirName) -> do
    let path = dirName </> fileName
    forM_ inputCalculusTypes $ \ inputType -> do
      let inputTypeAsString = toLower <$> show inputType
      let file = readCalculusFile inputTypeAsString path
      forM_ calculusTypes $ \ outputType -> do
        let outputTypeAsString = toLower <$> show outputType
        it (inputTypeAsString </> outputTypeAsString </> path) $
          safeIOToIO (toCombinatorsText outputType inputType <$> file) `goldenShouldIO` buildAbsoluteCalculusExtFileName inputTypeAsString outputTypeAsString path
