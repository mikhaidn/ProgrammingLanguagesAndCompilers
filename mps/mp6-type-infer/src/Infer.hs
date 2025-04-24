{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use infix" #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Infer where

import Common
import Control.Monad.Except (ExceptT (ExceptT), throwError)
import Control.Monad.Writer (listen)
import Data.Map.Strict as H (Map, empty, fromList, insert, lookup, singleton, union)

{- question 1: fresh instance function -}

freshInst :: PolyTy -> Infer MonoTy
freshInst (Forall qVars tau) = do
  freshVars <- mapM (const freshTau) qVars
  let subst = H.fromList (zip qVars freshVars)
  return $ apply subst tau

{- question 2: occurs check -}

occurs :: VarId -> MonoTy -> Bool
occurs i tau = (elem i (freeVars tau))

{- question 3: unification -}

unify :: [Constraint] -> Infer Substitution
unify [] = return substEmpty
unify ((s :~: t) : cs)
  | s == t = unify cs
  | TyVar v <- s = bindVar v t cs -- Orient/eliminate if s is a variable
  | TyVar v <- t = bindVar v s cs
  | TyConst c1 ts1 <- s,
    TyConst c2 ts2 <- t -- Decompose rule
    =
    if c1 == c2
      then unify (zipWith (:~:) ts1 ts2 ++ cs)
      else throwError $ Can'tMatch s t
  | otherwise = throwError $ Can'tMatch s t
  where
    -- Helper function to handle variable binding
    bindVar v t cs
      | v `occurs` t = throwError $ InfiniteType v t
      | otherwise = do
        let subst = substInit v t
        appCS <- unify (map (apply subst) cs)
        return (appCS `substCompose` subst)

{- question 4: type inference -}

infer :: TypeEnv -> Exp -> Infer MonoTy
infer env (ConstExp c) = freshInst (constTySig c)
infer env (VarExp x) = do
  case H.lookup x env of
    Nothing -> throwError (LookupError x)
    Just s -> freshInst s

infer env (LetExp x e1 e2) = do
  (tau1, constraints) <- listen (infer env e1)
  subst <- unify constraints
  let tau1' = apply subst tau1
      env' = H.insert x (gen (apply subst env) tau1') (apply subst env)
  (tau, phi2) <- listen (infer env' e2)
  sigma2 <- unify phi2
  return (apply sigma2 tau)

infer env (BinOpExp op e1 e2) = do
  tau1 <- infer env e1
  tau2 <- infer env e2
  tau <- freshTau
  opType <- freshInst (binopTySig op)
  constrain opType (funTy tau1 (funTy tau2 tau))
  return tau

infer env (MonOpExp op e) = do
  tau1 <- infer env e
  tau <- freshTau
  opType <- freshInst (monopTySig op)
  constrain opType (funTy tau1 tau)
  return tau

infer env (IfExp e1 e2 e3) = do
  tau1 <- infer env e1
  tau2 <- infer env e2
  tau3 <- infer env e3
  constrain tau1 boolTy
  constrain tau2 tau3
  return tau2

infer env (FunExp x e) = do
  tau1 <- freshTau
  tau2 <- infer (H.insert x (polyTyOfMonoTy tau1) env) e
  return (funTy tau1 tau2)

infer env (AppExp e1 e2) = do
  tau1 <- infer env e1
  tau2 <- infer env e2
  tau <- freshTau
  constrain tau1 (funTy tau2 tau)
  return tau

infer env (LetRecExp f x e1 e2) = do
  tau1 <- freshTau
  tau2 <- freshTau
  let env' = H.insert f (polyTyOfMonoTy (funTy tau1 tau2)) (H.insert x (polyTyOfMonoTy tau1) env)
  (tau3, phi1) <- listen (infer env' e1)
  sigma <- unify ((tau2 :~: tau3) : phi1)
  let tau1' = apply sigma tau1
      tau2' = apply sigma tau2
      env'' = H.insert f (gen (apply sigma env) (funTy tau1' tau2')) (apply sigma env)
  (tau, phi2) <- listen (infer env'' e2)
  return (apply sigma tau)

inferInit :: TypeEnv -> Exp -> Infer PolyTy
inferInit env e = do
  (tau, constraints) <- listen $ infer env e
  substitution <- unify constraints
  return $ quantifyMonoTy $ apply substitution tau

inferDec :: TypeEnv -> Dec -> Infer (TypeEnv, PolyTy)
inferDec env (AnonDec e') = do
  tau <- inferInit env e'
  return (env, tau)
inferDec env (LetDec x e') = do
  tau <- inferInit env (LetExp x e' (VarExp x))
  return (H.insert x tau env, tau)
inferDec env (LetRec f x e') = do
  tau <- inferInit env (LetRecExp f x e' (VarExp f))
  return (H.insert f tau env, tau)