module Main where

import           HelVM.HelTC.Calculators.LC.Calculator

import           HelVM.HelIO.Control.Control

import           AppOptions

import           Options.Applicative

main :: IO ()
main = run =<< execParser opts where
  opts = info (optionParser <**> helper)
      ( fullDesc
     <> header "HelTC - Haskellish Esoteric Lambda True Calculator to Esoteric Languages implemented in Haskell "
     <> progDesc "" )

run :: AppOptions -> IO ()
run (AppOptions printLogs parserType generatorType filePath) =
  putTextLn =<< controlTToIO printLogs (assembleFile generatorType parserType filePath)
