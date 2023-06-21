module HelVM.HelTC.Calculators.Combinators.Zot.Calculator (
  evalSource,
  evalWithFormat,
) where

import           HelVM.HelTC.Calculators.Combinators.Zot.Evaluator
import           HelVM.HelTC.Calculators.Combinators.Zot.Expression
import           HelVM.HelTC.Calculators.Combinators.Zot.Parser

import           HelVM.HelTC.Calculator.API.IOTypes

import           HelVM.HelTC.Calculator.Types.FormatType

import           HelVM.HelIO.Containers.Extra
import           HelVM.HelIO.Control.Safe

import           HelVM.HelIO.Digit.Digitable
import           HelVM.HelIO.Digit.ToDigit

import           HelVM.HelIO.ListLikeExtra

import           HelVM.HelIO.IO.BIO
import           HelVM.HelIO.IO.Console

import           Control.Monad.Writer.Lazy

import qualified Data.Text.Lazy                                     as LT

evalSource :: BIO m => FormatType -> Source -> m Output
evalSource t s = wGetContentsText >>= evalWithFormat t s

evalWithFormat :: MonadSafe m => FormatType -> Source -> LT.Text -> m Output
evalWithFormat BinaryLabel source input = pure $ showFoldable $ evalInternal source input
evalWithFormat TextLabel   source input = (makeAsciiText28 <$> convert <$> evalInternal source) <$> showExpressionList =<< stringToDL (toString input)

evalInternal :: Source -> LT.Text -> ExpressionDList
evalInternal source input = eval $ fromStrict source <> input

eval :: LT.Text  -> ExpressionDList
eval = execWriter <$> runExpressionList <$> parse
