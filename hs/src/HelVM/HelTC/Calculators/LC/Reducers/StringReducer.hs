module HelVM.HelTC.Calculators.LC.Reducers.StringReducer (
  reduceStringForRootList,
  reduceStringForRoot,
) where

import           HelVM.HelTC.Calculators.LC.Lambda

import           Data.ByteString                   (unpack)

reduceStringForRootList :: InstructionList -> InstructionList
reduceStringForRootList = fmap reduceStringForRoot

reduceStringForRoot :: Instruction -> Instruction
reduceStringForRoot (Def n f) = Def n $ reduceString f
reduceStringForRoot (Eval  f) = Eval  $ reduceString f

reduceString :: Lambda -> Lambda
reduceString (App f g) = App (reduceString f) (reduceString g)
reduceString (Abs n f) = Abs n $ reduceString f
reduceString (Str l)   = lambdaFromString l
reduceString       l   = l

lambdaFromString :: String -> Lambda
lambdaFromString = List . map lambdaFromIntegral . unpack . encodeUtf8 . toText
