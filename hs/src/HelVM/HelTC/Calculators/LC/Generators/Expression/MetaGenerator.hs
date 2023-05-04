module HelVM.HelTC.Calculators.LC.Generators.Expression.MetaGenerator where

import           HelVM.HelTC.Calculators.LC.Generators.GeneratorUtil

import           HelVM.HelTC.Calculators.LC.Lambda

import           HelVM.HelTC.Calculator.Value

generateCodeForIL :: InstructionList -> Text
generateCodeForIL il = unlines $ generateCodeForInstruction <$> il

generateCodeForInstruction :: Instruction -> Text
generateCodeForInstruction (Def i l) = generateCodeForDefinition i l
generateCodeForInstruction (Eval  l) = generateCodeForLambda l

generateCodeForDefinition :: Identifier -> Lambda -> Text
generateCodeForDefinition i l = ": " <> i <> " " <> generateCodeForLambda l

generateCodeForLambda  :: Lambda -> Text
generateCodeForLambda S              = "S"
generateCodeForLambda K              = "K"
generateCodeForLambda I              = "I"
generateCodeForLambda a@(App _ _)    = generateCodeForApp a
generateCodeForLambda f@(Abs _ _)    = generateCodeForAbs f
generateCodeForLambda (AbsRe l f)    = generateCodeForAbsRe l f
generateCodeForLambda (Var v)        = v
generateCodeForLambda (Nat   n)      = show n
generateCodeForLambda (Int (SN s n)) = showSign s <> show n
generateCodeForLambda (Dec s n)      = show s <> show n
generateCodeForLambda (List  l)      = generateCodeForList l
generateCodeForLambda (Str   s)      = generateCodeForSrt s

generateCodeForApp :: Lambda -> Text
generateCodeForApp (App f a) = generateCodeForApp f <> " " <> par generateCodeForLambda  a
generateCodeForApp e         = par generateCodeForLambda  e

generateCodeForAbs :: Lambda -> Text
generateCodeForAbs (Abs p e) = "\\ " <> p <> " " <> generateCodeForAbs e
generateCodeForAbs e         = generateCodeForLambda  e

generateCodeForAbsRe :: IdentifierList -> Lambda -> Text
generateCodeForAbsRe il f = "(lambda (" <> show il <> ")" <> generateCodeForLambda f <> ")"

generateCodeForList :: LambdaList-> Text
generateCodeForList l   = "[" <> show l <> "]"

generateCodeForSrt :: String -> Text
generateCodeForSrt s = "\"" <> toText s <> "\""
