module Main where

import           HelVM.HelTC.Calculators.Evaluator

import           HelVM.HelTC.Calculators.API.CalculatorParams

import           HelVM.HelIO.Control.Control

import           HelVM.HelIO.Extra

import qualified AppOptions                                   as App

import           Options.Applicative

import qualified System.IO                                    as IO

main :: IO ()
main = runApp =<< execParser opts where
  opts = info (App.optionParser <**> helper)
      ( fullDesc
     <> header "HelTC - Haskellish Esoteric Lambda True Calculator to Esoteric Languages implemented in Haskell "
     <> progDesc "" )

runApp:: App.AppOptions -> IO ()
runApp = \ o -> do
  hSetBuffering stdout IO.NoBuffering
  source <- readSourceFile (App.exec o) (App.file o)
  run (App.printLogs o) (App.calculatorParams o) source

readSourceFile :: Bool -> String -> IO Text
readSourceFile True = pure . toText
readSourceFile _    = readFileTextUtf8

run :: Bool -> CalculatorParams -> Text  -> IO ()
run = \ printLogs calculatorParams  source ->
  controlTToIO printLogs (evalSource calculatorParams source)
