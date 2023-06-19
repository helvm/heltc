module HelVM.HelTC.Calculators.Lambda.Reducers.SkiReducer (
  reduceSki,
) where

import           HelVM.HelTC.Calculators.Lambda.Lambda

reduceSki :: Lambda -> Lambda
reduceSki = fst . flip reduceSkiWithIndex 0

reduceSkiWithIndex :: Lambda -> Int -> (Lambda , Int)
reduceSkiWithIndex (App f a) n = reduceApp f a n
reduceSkiWithIndex S         n = (reduceS n       , n + 1)
reduceSkiWithIndex K         n = (reduceK n       , n + 1)
reduceSkiWithIndex I         n = (reduceI n       , n + 1)
reduceSkiWithIndex _         _ = error "reduceSkiWithIndex"

reduceApp :: Lambda -> Lambda -> Int -> (Lambda , Int)
reduceApp f a n = (App f' a' , n'') where
  (a', n'') = reduceSkiWithIndex a n'
  (f', n')  = reduceSkiWithIndex f n

reduceS :: Int -> Lambda
reduceS n = fx $ fy $ fz $ x `App` z `App` App y z where
  (fx, x) = makeFunWithVarX n
  (fy, y) = makeFunWithVarY n
  (fz, z) = makeFunWithVarZ n

reduceK :: Int -> Lambda
reduceK n = fx $ fy x where
  (fx, x) = makeFunWithVarX n
  (fy, _) = makeFunWithVarY n

reduceI :: Int -> Lambda
reduceI n = fx x where
  (fx , x) = makeFunWithVarX n

makeFunWithVarX :: Int -> (Lambda -> Lambda , Lambda)
makeFunWithVarX = makeFunWithVar "x"

makeFunWithVarY :: Int -> (Lambda -> Lambda , Lambda)
makeFunWithVarY = makeFunWithVar "y"

makeFunWithVarZ :: Int -> (Lambda -> Lambda , Lambda)
makeFunWithVarZ = makeFunWithVar "z"

makeFunWithVar :: Text -> Int -> (Lambda -> Lambda , Lambda)
makeFunWithVar t a = (Abs &&& Var) (t <> show a)
