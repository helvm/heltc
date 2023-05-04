module HelVM.HelTC.Calculators.LC.Generators.ExpressionGenerator where

import           HelVM.HelTC.Calculators.LC.API.ParserType

import qualified HelVM.HelTC.Calculators.LC.Generators.Expression.MetaGenerator     as MetaGenerator
import qualified HelVM.HelTC.Calculators.LC.Generators.Expression.SymbolicGenerator as SymbolicGenerator

import           HelVM.HelTC.Calculators.LC.Lambda

generateCodeForIL :: ParserType -> InstructionList -> Text
generateCodeForIL Meta     = MetaGenerator.generateCodeForIL
generateCodeForIL Symbolic = SymbolicGenerator.generateCodeForIL
