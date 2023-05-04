{-# LANGUAGE StrictData #-}
module AppOptions where

import           HelVM.HelTC.Calculators.LC.API.GeneratorType
import           HelVM.HelTC.Calculators.LC.API.ParserType

import           BoolTypes

import           Options.Applicative

optionParser :: Parser AppOptions
optionParser = AppOptions
  <$> switch       (  long    "print-logs"
                   <> short   'L'
                   <> help    "Pring logs to strerr"
                   <> showDefault
                   )
  <*> option auto  (  long    "parserType"
                   <> short   'p'
                   <> metavar "[ParserType]"
                   <> help   ("ParserType: " <> show parserTypes)
                   <> value    defaultParserType
                   <> showDefault
                   )
  <*> option auto  (  long    "generatorType"
                   <> short   'g'
                   <> metavar "[GeneratorType]"
                   <> help   ("GeneratorType: " <> show generatorTypes)
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
  { printLogs     :: !PrintLogs
  , parserType    :: !ParserType
  , generatorType :: !GeneratorType
  , file          :: !String
  }
