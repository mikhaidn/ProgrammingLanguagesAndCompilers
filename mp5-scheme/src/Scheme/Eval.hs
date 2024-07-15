{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}

module Scheme.Eval where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.HashMap.Strict as H (HashMap, empty, fromList, insert, lookup, union)
import Scheme.Core
import Prelude hiding (lookup)

-- ### Evaluation helpers

-- Evaluates a symbol to string
-- Throws an error if value is not a symbol
-- Examples:
--   getSym (Symbol "x")  ==> "x"
--   getSym (Number 1)    ==> Not a symbol: x
getSym :: Val -> EvalState String
getSym (Symbol x) = return x
getSym v = throwError $ NotASymbol v

-- `let` and `let*`
getBinding :: Val -> EvalState (String, Val)
getBinding (Pair c (Pair e Nil)) = liftM2 (,) (getSym c) (eval e)
getBinding v = throwError $ NotAListOfTwo v

-- Evaluates a list of two to a tuple
-- Throws an error if value is not a list of two
-- This is useful in special form `cond`, since each clause
-- is expected to be exactly a two-element list
getListOf2 :: Val -> EvalState (Val, Val)
getListOf2 (Pair c (Pair e Nil)) = return (c, e)
getListOf2 v = throwError $ NotAListOfTwo v

-- Evaluates a value representing a list into an actual list
getList :: Val -> EvalState [Val]
getList Nil = return []
getList (Pair v1 v2) =
  do
    xs <- getList v2
    return (v1 : xs)
getList e = throwError $ InvalidSpecialForm "special" e

--- ### Keywords

-- When evaluating special forms, a list form starting with a keyword
-- is expected to match the special form syntax.
keywords :: [String]
keywords =
  [ "define",
    "lambda",
    "cond",
    "let",
    "let*",
    "define-macro",
    "quasiquote",
    "unquote"
  ]

-- ### The monadic evaluator
-- Unlike evaluators in previous MPs, `eval` does not take any environment!
-- This is because the environment is encapsulated in the `EvalState` monad.
-- To access the environment, all you have to do is `get`, `modify` or `put`!
eval :: Val -> EvalState Val
-- Self-evaluating expressions
-- TODO: What's self-evaluating?
eval v@(Number _) = return v
eval v@(Boolean _) = return v
-- Symbol evaluates to the value bound to it
-- TODO
eval (Symbol sym) = do
  env <- get -- Get the current environment
  case H.lookup sym env of
    Just val -> return val -- Return the value if found
    Nothing -> throwError $ UndefSymbolError "sym" -- Handle unbound symbol case

-- Function closure is also self-evaluating
eval v@(Func _ _ _) = return v
-- We check to see if the pair is a "proper list". If it is,
-- then we try to evaluate it, as one of the following forms:
-- 1. Special form (`define`, `let`, `let*`, `cond`, `quote`, `quasiquote`,
--    `unquote`, `define-macro`, ...)
-- 2. Macro expansion (Macro)
-- 3. Function application (Func)
-- 4. Primitive function application (PrimFunc)
eval expr@(Pair v1 v2) = case flattenList expr of
  Left _ -> throwError $ InvalidExpression expr
  Right lst -> evalList lst
    where
      --- Evaluator for forms
      invalidSpecialForm :: String -> EvalState e
      invalidSpecialForm frm = throwError $ InvalidSpecialForm frm expr

      evalList :: [Val] -> EvalState Val

      evalList [] = throwError $ InvalidExpression expr
      -- quote
      -- TODO
      evalList [Symbol "quote", e] = return e
      -- unquote (illegal at surface evaluation)
      -- TODO: since surface-level `unquote` is illegal, all you need to do is
      -- to throw a diagnostic
      evalList [Symbol "unquote", e] = throwError $ InvalidSpecialForm "unquote must be used within quasiquote" e
      evalList [Symbol "quasiquote", e] = evalQuasi 1 e
        where
          evalQuasi :: Int -> Val -> EvalState Val
          evalQuasi 0 (Pair (Symbol "unquote") v) = throwError $ UnquoteNotInQuasiquote v
          evalQuasi 1 (Pair (Symbol "unquote") (Pair v Nil)) = eval v
          evalQuasi n (Pair (Symbol "quasiquote") (Pair v Nil)) =
            do
              v' <- evalQuasi (n + 1) v
              return $ Pair (Symbol "quasiquote") (Pair v' Nil)
          evalQuasi n (Pair (Symbol "unquote") (Pair v Nil)) =
            do
              v' <- evalQuasi (n -1) v
              return $ Pair (Symbol "unquote") (Pair v' Nil)
          evalQuasi n (Pair x y) = Pair <$> evalQuasi n x <*> evalQuasi n y
          evalQuasi _ v = return v

      -- cond
      evalList (Symbol "cond" : clauses) = evalCond clauses
        where
          evalCond [] = return Void 
          evalCond (clause : rest) = do
            (cond, expr) <- getListOf2 clause
            condValue <- eval cond
            case condValue of
              Boolean False -> evalCond rest 
              Boolean True -> eval expr 
              Symbol "else" ->
                if null rest
                  then eval expr
                  else throwError $ InvalidSpecialForm "Bad" expr
              _ -> eval expr -- For truthy values, evaluate and return the expression

      -- let
      evalList [Symbol "let", bindings, body] =
        do
          env <- get
          bindingsList <- getList bindings
          evaluatedBindings <- mapM getBinding bindingsList
          let newEnv = foldl (\acc (var, val) -> H.insert var val acc) env evaluatedBindings
          put newEnv
          result <- eval body
          put env
          return result

      -- lambda
      -- TODO: Handle `lambda` here. Use pattern matching to match the syntax
      evalList [Symbol "lambda", args, body] =
        do
          env <- get
          argList <- getList args
          let argNames = map (\(Symbol s) -> s) argList
          return $ Func argNames body env

      -- define function
      evalList [Symbol "define", Pair (Symbol fname) args, body] =
        do
          env <- get
          argList <- getList args
          val <- (\argVal -> Func argVal body env) <$> mapM getSym argList
          modify $ H.insert fname val
          return Void

      -- define variable
      -- TODO: Handle `define` for variables here. Use pattern matching
      evalList [Symbol "define", (Symbol varname), expr] =
        do
          env <- get
          val <- eval expr
          modify $ H.insert varname val
          return Void

      -- define-macro
      evalList [Symbol "define-macro", Pair (Symbol fname) params, body] =
        do
          env <- get
          paramList <- getList params
          val <- (\argVal -> Macro argVal body) <$> mapM getSym paramList
          modify $ H.insert fname val
          return Void

      -- invalid use of keyword, throw a diagnostic
      evalList (Symbol sym : _) | elem sym keywords = invalidSpecialForm sym
      -- application
      evalList (fexpr : args) =
        do
          f <- eval fexpr
          apply f args
eval val = throwError $ InvalidExpression val

-- Function application
apply :: Val -> [Val] -> EvalState Val
-- Function
apply (Func params body closureEnv) args =
  do
    argVals <- mapM eval args
    currentEnv <- get
    let newEnv = H.fromList (zip params argVals) `H.union` closureEnv `H.union` currentEnv
    put newEnv
    result <- eval body
    put currentEnv
    return result

-- Macro
apply (Macro params body) args = do
  currentEnv <- get -- Step 2: Save the environment
  let newEnv = H.fromList (zip params args) `H.union` currentEnv
  put newEnv
  expanded <- eval body
  put currentEnv
  eval expanded

-- Primitive
apply (PrimFunc p) args =
  do
    argVals <- mapM eval args
    p argVals
-- Other values are not applicable
apply f args = throwError $ CannotApply f args
