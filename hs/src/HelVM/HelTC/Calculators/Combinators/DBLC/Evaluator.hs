module HelVM.HelTC.Calculators.Combinators.DBLC.Evaluator where

import           HelVM.HelTC.Calculators.Combinators.DBLC.Generator
import           HelVM.HelTC.Calculators.Combinators.DBLC.Parser
import           HelVM.HelTC.Calculators.Combinators.DBLC.Term

import           HelVM.HelIO.Control.Safe

import           Control.Monad.Except

import qualified Data.List                                          as List
import qualified Data.Map.Strict                                    as Map

import qualified Relude.Unsafe                                      as Unsafe

-- The proof environment monad.
-- Contains a map from de bruijn levels to terms
-- a context containing the types for de bruijn indices,
-- and an integer representing the level.

type ExceptLegacy = ExceptT String

type Proof = ExceptLegacy (ReaderT [Term] (StateT (Map.Map Int (Term, Term)) (State Int)))

runProof :: Proof a -> EitherLegacy a
runProof p = fst $ evalState (runStateT (runReaderT (runExceptT p) []) Map.empty) 0

-- ======= Evaluation =======

-- Reduce a term to weak head normal form.
whnf' :: Bool -> Term -> Proof Term
whnf' names ee = spine ee [] where
  spine :: Term -> [Term] -> Proof Term
  spine (f :% a) as = spine f (a:as)
  spine (Lam _t z) (u:as) = spine (sub u 0 z) as
  -- Eta conversion
  spine (Lam t (tp :% Var 0)) [] =
            if freeIn tp 0
            then pure (Lam t (tp :% Var 0))
            else spine (sub (Var 0) 0 tp) []
  spine (Level i) as =
    if names -- Should names/levels be removed
    then do
      tbl <- get
      case Map.lookup i tbl of
        Nothing -> throwError $ "Level " ++ show i ++ " not found in context (whnf)."
        Just t  -> spine (fst t) as
    else app (Level i) as
  spine f as = app f as
  app f as = pure $ List.foldl (:%) f as

whnf :: Term -> Proof Term
whnf = whnf' False

nwhnf :: Term -> Proof Term
nwhnf = whnf' True

-- Normal Form
nf' :: Term -> Proof Term
nf' ee = spine ee [] where
  spine (f :% a) as = spine f (a:as)
  -- Eta conversion
  spine (Lam t (tp :% Var 0)) [] =
            if freeIn tp 0
            then Lam <$> nf' t <*> nf' (tp :% Var 0)
            else spine (sub (Var 0) 0 tp) []
  spine (Lam t e) [] = Lam <$> nf' t <*> nf' e
  spine (Lam _t e) (u:as) = spine (sub u 0 e) as
  spine (Level i) as = do
        tbl <- get
        case Map.lookup i tbl of
          Nothing -> throwError $ "Level " ++ show i ++ " not found in context."
          Just t  -> spine (fst t) as
  spine f as = List.foldl (:%) f <$> mapM nf' as

nf :: Term -> Proof Term
nf d = do
  r <- nf' d
  if d == r
  then pure r
  else nf r

-- ======= Type Checking =======

infer :: Term -> Proof Term
infer t = do
  wt <- whnf t
  case wt of
    Level i -> do
      tbl <- get
      case Map.lookup i tbl of
           Nothing -> throwError $ "Level " ++ show i ++ " not found in context durring type inference."
           Just t' -> pure $ snd t'
    Var n -> do
      ctx <- ask
      case (ctx , n) of
        ([], _) -> throwError $ "Cannot infer term variable in empty context."
        (x:_g, 0) -> local Unsafe.tail $ do
                       check x U
                       pure (quote 0 x)
        (_:_g, _n) -> local Unsafe.tail $ do
                       ty <- infer (Var (n - 1))
                       pure (quote 0 ty)
    tr1 :% tr2 -> do
      ty1' <- infer tr1
      ty1  <- nwhnf ty1'
      case ty1 of
        Lam tp1 tp2 -> do
          check tr2 tp1
          pure (sub tr2 0 tp2)
        _ -> error "infer"
    Lam ty1 ty2 -> do
      check ty1 U
      local (ty1:) $ do
        check ty2 U
        pure U
    U -> pure U

check :: Term -> Term -> Proof ()
check tr ty =
  case tr of
    Level i -> do
      tbl <- get
      case Map.lookup i tbl of
           Nothing -> throwError $ "Level " ++ show i ++ " not found in context durring type checking."
           Just (_, t)  -> do
             tnf  <- nf t
             tynf <- nf ty
             if tnf == tynf
             then pure ()
             else throwError $ "Type didn't match durring lookup."
    Var n -> do
      ctx <- ask
      case (ctx , n) of
        ([], _) -> throwError $ "Cannot check type of variable term in an empty context."
        (x:_g, 0) -> do
          xnf  <- nf (quote 0 x)
          tynf <- nf ty
          if tynf == xnf
          then do
            check ty U
            local Unsafe.tail $ check x U
          else throwError $ "Term does not have correct type."
        (_:_g, _) -> local Unsafe.tail $ check (Var (n - 1)) (sub (Var 0) 0 ty)
    Lam aty tr' -> do
      tyw <- nwhnf ty
      case tyw of
        Lam ty1 ty2 -> do
          ty1nf <- nf ty1
          atynf <- nf aty
          if ty1nf == atynf
          then local (ty1:) $ check tr' ty2
          else throwError $ "Type of lam annotation didn't match type annotation."
        U -> do
          check aty U
          local (aty:) $ check tr' U
        _ -> throwError $ "Lambdas can only be Lam or * types."
    tr1 :% tr2 -> do
      ity <- infer (tr1 :% tr2)
      tynf <- nf ty
      itynf <- nf ity
      if tynf == itynf
      then check ty U
      else throwError $ "Failed to unify at application."
    U -> do
      tyw <- nwhnf ty
      case tyw of
        U -> pure ()
        _ -> throwError $  "* can only have type *."

-- ======= Concrete Syntax =======

output :: Proof a -> String
output p = case runState (runStateT (runReaderT (runExceptT p) []) Map.empty) 0 of
    ((_, mp'), lvl) -> case toBin <$> fst <$> Map.lookup (lvl-1) mp' of
                         Just s  -> s
                         Nothing -> ""

checkProg :: String -> Proof ()
checkProg s = go (parse s [] []) where
  go :: [Term] -> Proof ()
  go (ty:tr:ctx) = do
    check tr ty
    lvl <- lift $ lift $ lift $ get
    _tbl <- get
    modify $ Map.insert lvl (tr, ty)
    lift $ lift $ lift $ modify (+1)
    go ctx
  go (_:[]) = throwError $ "Type is given without implementation."
  go [] = pure ()
