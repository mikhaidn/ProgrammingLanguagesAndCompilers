--- Given Code
--- ==========
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use if" #-}

module Lib where

import Data.Functor.Identity (Identity)
import Data.List (intercalate)
import System.IO (hFlush, hPutStr, hPutStrLn, stdout)
import Text.Parsec.Prim (ParsecT)
import Text.ParserCombinators.Parsec hiding (Parser)

--- The Types
--- ---------

data Stmt = Decl String [String] Exp
  deriving (Eq)

instance Show Stmt where
  show (Decl f params exp) = f ++ " " ++ intercalate " " params ++ " = " ++ (show exp)

data Exp
  = IntExp Integer
  | VarExp String
  | LamExp String Exp
  | IfExp Exp Exp Exp
  | OpExp String Exp Exp
  | AppExp Exp Exp
  deriving (Eq)

instance Show Exp where
  show (VarExp s) = s
  show (IntExp i) = show i
  show (LamExp x e) = "(\\" ++ x ++ " -> " ++ (show e) ++ ")"
  show (IfExp e1 e2 e3) =
    "(if "
      ++ show e1
      ++ " then "
      ++ show e2
      ++ " else "
      ++ show e3
      ++ ")"
  show (OpExp op e1 e2) = "(" ++ show e1 ++ " " ++ op ++ " " ++ show e2 ++ ")"
  show (AppExp f e) = show f ++ " " ++ show e

ctorShow :: Exp -> String
ctorShow (VarExp s) = "VarExp " ++ show s
ctorShow (IntExp i) = "IntExp " ++ show i
ctorShow (LamExp x e) = "LamExp " ++ show x ++ " (" ++ ctorShow e ++ ")"
ctorShow (IfExp e1 e2 e3) =
  "IfExp ("
    ++ ctorShow e1
    ++ ") ("
    ++ ctorShow e2
    ++ ") ("
    ++ ctorShow e3
    ++ ")"
ctorShow (OpExp op e1 e2) =
  "OpExp "
    ++ show op
    ++ " ("
    ++ ctorShow e1
    ++ ") ("
    ++ ctorShow e2
    ++ ")"
ctorShow (AppExp f e) = "AppExp (" ++ ctorShow f ++ ") (" ++ ctorShow e ++ ")"

--- Problems
--- ========

--- Manual Translation
--- ------------------

--- ### `factk :: Integer -> (Integer -> t) -> t`

factk :: Integer -> (Integer -> t) -> t
factk 0 k = k 1
factk n k = factk (n - 1) (\x -> k (n * x))

--- ### `evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t`

evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t
evenoddk (x : []) keven kodd = case (mod x 2) of
  0 -> keven x
  1 -> kodd x
evenoddk (x : xs) keven kodd = case (mod x 2) of
  0 -> evenoddk xs (\n -> keven (n + x)) kodd
  1 -> evenoddk xs keven (\n -> kodd (n + x))

--- Automated Translation
--- ---------------------

gensym :: Integer -> (String, Integer)
gensym i = ("v" ++ show i, i + 1)

--- ### Define `isSimple`

isSimple :: Exp -> Bool
isSimple (IntExp _) = True
isSimple (VarExp _) = True
isSimple (LamExp _ _) = True
isSimple (IfExp e1 e2 e3) = ((isSimple e1) && (isSimple e2) && (isSimple e3))
isSimple (OpExp _ e1 e2) = (isSimple e1) && (isSimple e2)
isSimple (AppExp _ _) = False

--- ### Define `cpsExp` - Overview

cpsExp :: Exp -> Exp -> Integer -> (Exp, Integer)
--- #### Define `cpsExp` for Integer and Variable Expressions
cpsExp e@(IntExp _) k i = (AppExp k e, i)
cpsExp e@(VarExp _) k i = (AppExp k e, i)
--- #### Define `cpsExp` for Application Expressions
cpsExp p@(AppExp f e) k i =
  case (isSimple e) of
    True -> (AppExp p k, i)
    False ->
      let (v, ni) = (gensym i)
       in (cpsExp e (LamExp v (AppExp (AppExp f (VarExp v)) k)) ni)
--- #### Define `cpsExp` for Operator Expressions
cpsExp p@(OpExp o e1 e2) k i = case (isSimple e1, isSimple e2) of
  (True, True) -> (AppExp k p, i)
  (False, True) ->
    let (v, ni) = (gensym i)
     in (cpsExp e1 (LamExp v (AppExp k (OpExp o (VarExp v) e2))) ni)
  (True, False) ->
    let (v, ni) = (gensym i)
     in (cpsExp e2 (LamExp v (AppExp k (OpExp o e1 (VarExp v)))) ni)
  (False, False) ->
    let (v1, ni) = gensym i
        (v2, nii) = gensym ni
        (ne1, niii) = cpsExp e2 (LamExp v2 (AppExp k (OpExp o (VarExp v1) (VarExp v2)))) ni
     in cpsExp e1 (LamExp v1 ne1) niii
--- #### Define `cpsExp` for If Expressions

cpsExp p@(IfExp e1 e2 e3) k i =
  case (isSimple e1) of
    True ->
      let (nv, ni) = (cpsExp e2 k i)
          (nvv, nii) = (cpsExp e3 k ni)
       in (IfExp e1 nv nvv, i)
    False ->
      let (v, ni) = (gensym i)
          (nv, nii) = (cpsExp e2 k ni)
          (nvv, niii) = (cpsExp e3 k nii)
       in (cpsExp e1 (LamExp v (IfExp (VarExp v) nv nvv)) niii)

--- ### Define `cpsDecl`


cpsDecl :: Stmt -> Stmt
cpsDecl (Decl fname params e) = 
    let k = "k"
        (v, _) = cpsExp e (VarExp k) 0
   in Decl fname (params ++ [k]) v
