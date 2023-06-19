module HelVM.HelTC.Calculators.Lambda.Lambda where

import           HelVM.HelTC.Calculator.Value

import           Relude.Extra

-- | Constructions
lambdaFromChar :: Char -> Lambda
lambdaFromChar = lambdaFromIntegral . ord

lambdaFromIntegral :: Integral a => a -> Lambda
lambdaFromIntegral = Nat . fromIntegral

lambdaFromNonEmpty :: NonEmpty Lambda -> Lambda
lambdaFromNonEmpty = foldl1' App

lambdaFromVariable :: Text -> Lambda
lambdaFromVariable "S" = S
lambdaFromVariable "K" = K
lambdaFromVariable "I" = I
lambdaFromVariable  n  = Var n

-- | Constants
appPairVariable :: Lambda -> Lambda -> Lambda
appPairVariable l = App pairVariable . App l

falseVariable , pairVariable , succVariable , trueVariable :: Lambda
falseVariable = Var "F"
pairVariable  = Var ","
succVariable  = Var "@"
trueVariable  = Var "T"

-- | Types
type InstructionList = [Instruction]

data Instruction =
    Eval Lambda
  | Def Identifier Lambda
  deriving stock (Eq , Ord , Read , Show)

type LambdaList = [Lambda]

infixl 9 `App`
infixr 8 `Abs`

data Lambda =
    S
  | K
  | I
  | App Lambda Lambda
  | Abs Identifier Lambda
  | AbsRe IdentifierList Lambda

  | Var Identifier
  | Nat Natural
  | Int SignNatural
  | Dec SignNatural SignNatural
  | List LambdaList
  | Str String
  deriving stock (Eq , Ord , Read , Show)



data SignNatural = SN Sign Natural
  deriving stock (Eq , Ord , Read , Show)
