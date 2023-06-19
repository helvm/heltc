module HelVM.HelTC.Calculators.Lambda.Generators.Impl.SymbolicGenerator (
  generateCodeForIL,
  generateCodeForLambda,
) where

import           HelVM.HelTC.Calculators.Lambda.Generators.GeneratorUtil

import           HelVM.HelTC.Calculators.Lambda.Lambda

import           HelVM.HelTC.Calculator.Value

generateCodeForIL :: InstructionList -> Text
generateCodeForIL il = unlines $ generateCodeForInstruction <$> il

generateCodeForInstruction :: Instruction -> Text
generateCodeForInstruction (Def i l) = generateCodeForDefinition i l
generateCodeForInstruction (Eval  l) = generateCodeForLambda l

generateCodeForDefinition :: Identifier -> Lambda -> Text
generateCodeForDefinition i l = "(define "  <> i <> " " <> generateCodeForLambda l <> ")"

generateCodeForLambda :: Lambda -> Text
generateCodeForLambda S              = "S"
generateCodeForLambda K              = "K"
generateCodeForLambda I              = "I"
generateCodeForLambda a@(App _ _)    = generateCodeForApp a
generateCodeForLambda f@(Abs _ _)    = "(lambda (" <> generateCodeForAbs f <> ")"
generateCodeForLambda (AbsRe l f)    = generateCodeForAbsRe l f
generateCodeForLambda (Var v)        = v
generateCodeForLambda (Nat   n)      = show n
generateCodeForLambda (Int (SN s n)) = showSign s <> show n
generateCodeForLambda (Dec s n)      = show s <> show n
generateCodeForLambda (List  l)      = generateCodeForList l
generateCodeForLambda (Str   s)      = generateCodeForSrt s

generateCodeForApp :: Lambda -> Text
generateCodeForApp (App f a) = applyBrackets (generateCodeForApp f <> " " <> generateCodeForLambda a)
generateCodeForApp e         = generateCodeForLambda  e

generateCodeForAbs :: Lambda -> Text
generateCodeForAbs (Abs p e) = p <> " " <> generateCodeForAbs e
generateCodeForAbs e         = ") " <> generateCodeForLambda e

generateCodeForAbsRe :: IdentifierList -> Lambda -> Text
generateCodeForAbsRe il f = "(lambda (" <> unwords il <> ") " <> generateCodeForLambda f <> ")"

generateCodeForList :: LambdaList -> Text
generateCodeForList l   = "(list " <> showList l <> ")"

showList :: Show a => [a] -> Text
showList l = unwords $ show <$> l

generateCodeForSrt :: String -> Text
generateCodeForSrt s = literalBrackets $ "\"" <> toText s <> "\""

literalBrackets :: Text -> Text
--literalBrackets s = s
literalBrackets = applyBrackets

applyBrackets :: Text -> Text
--applyBrackets s = "[" <> s <> "]"
applyBrackets s = "(" <> s <> ")"
