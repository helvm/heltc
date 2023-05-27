{-# LANGUAGE StrictData #-}
module AppOptions where

import           HelVM.HelTC.Calculators.LC.API.ILType
import           HelVM.HelTC.Calculators.LC.API.LambdaType

import           BoolTypes

import           Options.Applicative

optionParser :: Parser AppOptions
optionParser = AppOptions
  <$> switch       (  long    "print-logs"
                   <> short   'L'
                   <> help    "Pring logs to strerr"
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
  <*> argument str (  metavar "FILE"
                   <> help   "File to calculate"
                   <> showDefault
                   )

-- | Methods

-- | Types

data AppOptions = AppOptions
  { printLogs  :: !PrintLogs
  , ilType     :: !ILType
  , lambdaType :: !LambdaType
  , file       :: !String
  }
