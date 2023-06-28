module HelVM.HelTC.Calculators.Combinators.DBLC.Main where

import           HelVM.HelTC.Calculators.Combinators.DBLC.Evaluator

import           HelVM.HelIO.Control.Message
import           HelVM.HelIO.Extra

-- Main program
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  case args of
   name:_ -> proveFile name
   _      -> putStrLn "No file provided."

proveFile :: String -> IO ()
proveFile f | endQ f = do
               fileContents <- readFileTextUtf8 f
               let res = checkProgram fileContents
               case runProof res of
                 Right () -> do putTextLn "Checking Successful!"
                                putTextLn $ output res
                 Left e   -> putTextLn $ errorsToText e
            | otherwise = proveFile (f ++ extention)

endQ :: String -> Bool
endQ s = extention == reverse (take (length extention) (reverse s))

extention :: [Char]
extention = ".dblc"
