{-# LANGUAGE StrictData #-}
module AppOptions where

import           Lang

import           Options.Applicative

optionParser :: Parser AppOptions
optionParser = AppOptions
  <$> option auto  (  long    "lang"
                   <> short   'l'
                   <> metavar "[LANG]"
                   <> help   ("Language to interpret " <> show langs)
                   <> value    defaultLang
                   <> showDefault
                   )
  <*> switch       (  long    "eval"
                   <> short   'e'
                   <> help    "Exec"
                   <> showDefault
                   )
  <*> argument str (  metavar "FILE")

data AppOptions = AppOptions
  { lang :: !Lang
  , exec :: !Exec
  , file :: !String
  }

type Exec          = Bool
