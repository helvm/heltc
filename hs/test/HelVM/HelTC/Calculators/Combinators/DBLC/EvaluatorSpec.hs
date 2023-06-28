module HelVM.HelTC.Calculators.Combinators.DBLC.EvaluatorSpec where

import           HelVM.HelTC.Calculators.Combinators.DBLC.Evaluator
import           HelVM.HelTC.Calculators.Combinators.DBLC.FileExtra

import           HelVM.HelIO.IO.MockIO

--import           HelVM.HelIO.ZipA

import           HelVM.GoldenExpectations

--import           Data.Char

import           System.FilePath.Posix

import           Test.Hspec                                         (Spec, describe, it)

spec :: Spec
spec =
  describe "eval" $
    describe "original" $ forM_ (
      [ "helloWorld"
      ]
      ) $ \(fileName) -> do
        let filePath = "original" </> fileName
        let file = readDBLCFile filePath
        let input = ""
        let path = filePath <> toString input
        let mock = ioExecMockIOWithInput input . evalSource =<< file :: IO MockIOData
        describe path $ do
          it ("output" </> path) $
            calculateOutput <$> mock `goldenShouldIO` buildAbsoluteDBLCOutFileName path
          it ("logged" </> path) $
            calculateLogged <$> mock `goldenShouldIO` buildAbsoluteDBLCLogFileName path
