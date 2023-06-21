{-# LANGUAGE StrictData #-}
module AppOptions where

import qualified HelVM.HelTC.Calculators.API.CalculatorParams            as Params
import           HelVM.HelTC.Calculators.API.CalculatorType

import           HelVM.HelTC.Calculators.Combinators.API.CombinatorsType

import           HelVM.HelTC.Calculators.Lambda.API.ILType
import           HelVM.HelTC.Calculators.Lambda.API.LambdaType

import           BoolTypes

import           Options.Applicative

optionParser :: Parser AppOptions
optionParser = AppOptions
  <$> switch       (  long    "print-logs"
                   <> short   'L'
                   <> help    "Pring logs to strerr"
                   <> showDefault
                   )
  <*> option auto  (  long    "calculatorType"
                   <> short   'c'
--                   <> metavar "[LANG]"
                   <> help   ("Language to interpret " <> show calculatorTypes)
                   <> value    defaultCalculatorType
                   <> showDefault
                   )
  <*> option auto  (  long    "ilType"
                   <> short   'p'
                   <> metavar "[ILType]"
                   <> help   ("ILType: " <> show parserTypes)
                   <> value    defaultParserType
                   <> showDefault
                   )
  <*> option auto  (  long    "lambdaType"
                   <> short   'g'
                   <> metavar "[LambdaType]"
                   <> help   ("LambdaType: " <> show generatorTypes)
                   <> value    defaultGeneratorType
                   <> showDefault
                   )
  <*> option auto  (  long    "combinatorsType"
                   <> short   'g'
                   <> metavar "[CombinatorsType]"
                   <> help   ("CombinatorsType: " <> show combinatorsTypes)
                   <> value    defaultCombinatorsType
                   <> showDefault
                   )
  <*> switch       (  long    "eval"
                   <> short   'e'
                   <> help    "Exec"
                   <> showDefault
                   )
  <*> argument str (  metavar "FILE"
                   <> help   "File to calculate"
                   <> showDefault
                   )

-- | Method to generate

calculatorParams :: AppOptions -> Params.CalculatorParams
calculatorParams = \ o -> case calculatorType o of
  Lambda      -> Params.Lambda (lambdaType o) (ilType o)
  Combinators -> Params.Combinators (combinatorsType o)


-- | Types

data AppOptions = AppOptions
  { printLogs       :: !PrintLogs
  , calculatorType  :: !CalculatorType
  , ilType          :: !ILType
  , lambdaType      :: !LambdaType
  , combinatorsType :: !CombinatorsType
  , exec            :: !Exec
  , file            :: !String
  }
