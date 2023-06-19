{-# LANGUAGE FlexibleContexts #-}
module HelVM.HelTC.Calculators.Blynn.Lambda where

import           HelVM.HelIO.SwitchEnum

data Com = S | K | I | B | C | T

data CL = Lf Com | CL :# CL | Ext String

type VarId = String
data LC = Var VarId | Lam VarId LC | LC :@ LC | Other Com

deLam :: LC -> LC
deLam t = case t of
  Lam v u -> case deLam u of
    Var w | v == w    -> Other I
          | otherwise -> Other K :@ Var w
    Other c           -> Other K :@ Other c
    x :@ y            -> Other S :@ deLam (Lam v x) :@ deLam (Lam v y)
    _                 -> error "error in deLam"
  x :@ y  -> deLam x :@ deLam y
  _       -> t

rewrite :: LC -> CL
rewrite t = case deLam t of
  Other c -> Lf c
  x :@ y  -> rewrite x :# rewrite y
  _       -> error "error in rewrite"

reduce :: CL -> Maybe CL
reduce t = case t of
  Lf I :# x           -> Just x
  Lf K :# x :# _      -> Just x
  Lf T :# x :# y      -> Just (y :# x)
  Lf S :# x :# y :# z -> Just (x :# z :# (y :# z))
  Lf B :# x :# y :# z -> Just (x      :# (y :# z))
  Lf C :# x :# y :# z -> Just (x :# z :#  y      )
  _                   -> Nothing

normalize :: CL -> CL
normalize t0 = down t0 [] up1 where
  down t args k = case t of
    x :# y -> down x (y:args) k
    _      -> k t args

  up1 t args = case reduce t of
    Just t' -> down t' args up1
    Nothing -> case args of
      []   -> down t [] up2
      a:as -> up1 (t :# a) as

  up2 t args = case args of
    []   -> t
    a:as -> up2 (t :# normalize a) as

run :: CL -> CL
run t0 = down t0 [] where
  down t args = case t of
    x :# y -> down x (y:args)
    _      -> up t args

  up t args = case reduce t of
    Just t' -> down t' args
    Nothing -> case args of
      []   -> t
      a:as -> up (t :# a) as

runM :: Monad m => (CL -> m (Maybe CL)) -> CL -> m CL
runM f t0 = down t0 [] where
  down t args = case t of
    x :# y -> down x (y:args)
    _      -> up t args

  up t args = do
    m <- f t
    case m of
      Just t' -> down t' args
      Nothing -> case args of
        []   -> pure t
        a:as -> up (t :# a) as

encodeChar :: Char -> CL
encodeChar c = go (fromEnum c) where
  go 0 = Lf K
  go n = Lf K :# (Lf T :# go (n - 1))

decodeChar :: CL -> Char
decodeChar t0 = unsafeEnum $ execState (runM red $ t0 :# Ext "Z" :# Ext "S") 0 where
  red t = case t of
    Ext "S" :# a -> do
      modify (1+)
      pure $ Just $ a :# Ext "Z" :# Ext "S"
    _ -> pure $ reduce t

encode :: [Char] -> CL
encode ""     = Lf K
encode (c:cs) = Lf K :# (Lf C :# (Lf T :# encodeChar c) :# encode cs)

decode :: CL -> [Char]
decode t0 = execState (runM red $ t0 :# Ext "nil" :# Ext "cons") id "" where
  red t = case t of
    Ext "cons" :# x :# xs -> do
      modify (. (decodeChar x:))
      pure $ Just $ xs :# Ext "nil" :# Ext "cons"
    _ -> pure $ reduce t

runCom :: CL -> String -> String
runCom t s = decode (run (t :# encode s))
