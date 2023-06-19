module HelVM.HelTC.Calculators.Lambda.Reducers.AbstractionReducer (
  reduceAbstractionsForRootList,
  reduceAbstractionsForRoot,
) where

import           HelVM.HelTC.Calculators.Lambda.Lambda

reduceAbstractionsForRootList :: InstructionList -> InstructionList
reduceAbstractionsForRootList = fmap reduceAbstractionsForRoot

reduceAbstractionsForRoot :: Instruction -> Instruction
reduceAbstractionsForRoot (Def n f) = Def n $ reduceAbstractions f
reduceAbstractionsForRoot (Eval  f) = Eval  $ reduceAbstractions f

reduceAbstractions :: Lambda -> Lambda
reduceAbstractions (App f g) = App (reduceAbstractions f) (reduceAbstractions g)
reduceAbstractions (Abs n f) = reduceToSki n f
reduceAbstractions  l        = l

--

onlySKI :: Lambda -> Bool
onlySKI S         = True
onlySKI K         = True
onlySKI I         = True
onlySKI (App f a) = onlySKI f && onlySKI a
onlySKI _         = False

reduceToSki :: Text -> Lambda -> Lambda
reduceToSki _ l
  | onlySKI l             = App K l
reduceToSki x0 (App f  a) = App (App S $ reduceToSki x0 f) $ reduceToSki x0 a
reduceToSki x0 (Abs x1 e) = reduceAbs x0 x1 e
reduceToSki x0 (Var x1  ) = reduceVar x0 x1
reduceToSki _   lambda    = lambda

reduceAbs :: Text -> Text -> Lambda -> Lambda
reduceAbs x0 x1 e
  | x0 == x1  = App K $ reduceToSki x0 e
  | otherwise = reduceToSki x0 $ reduceToSki x1 e

reduceVar :: Text -> Text -> Lambda
reduceVar x0 x1
  | x1 == x0  = I
  | otherwise = App K (Var x1)
