module HelVM.HelTC.Calculators.Combinators.DBLC.Main where

import           HelVM.HelTC.Calculators.Combinators.DBLC.Evaluator

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
               let res = checkProg $ toString fileContents
               case runProof res of
                 Right () -> do putStrLn "Checking Successful!"
                                putStrLn $ output res
                 Left e   -> putStrLn e
            | otherwise = proveFile (f ++ extention)

endQ :: String -> Bool
endQ s = extention == reverse (take (length extention) (reverse s))

extention :: [Char]
extention = ".dblc"
