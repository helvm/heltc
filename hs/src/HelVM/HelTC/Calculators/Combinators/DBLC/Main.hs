module HelVM.HelTC.Calculators.Combinators.DBLC.Main where

import           HelVM.HelTC.Calculators.Combinators.DBLC.Evaluator

-- Main program
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  case args of
   name:_ -> proveFile name
   _      -> putStrLn "No file provided."
