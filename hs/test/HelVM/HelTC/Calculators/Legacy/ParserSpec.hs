module HelVM.HelTC.Calculators.Legacy.ParserSpec (spec) where

import           HelVM.HelTC.Calculators.Legacy.API.CalculusType
import           HelVM.HelTC.Calculators.Legacy.FileUtil
import           HelVM.HelTC.Calculators.Legacy.Parser

import           HelVM.HelIO.Control.Safe
import           HelVM.HelIO.Extra
import           HelVM.HelIO.ZipA

import           HelVM.GoldenExpectations

import           Data.Char

import           System.FilePath.Posix

import           Test.Hspec                                      (Spec, describe, it)

spec :: Spec
spec = describe "parse" $ forM_ ((
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
      it (inputTypeAsString </> path) $
        (showP <$> (safeIOToIO $ parseLambdaText inputType <$> file)) `goldenShouldIO` buildAbsolutePathToIlFile inputTypeAsString path
