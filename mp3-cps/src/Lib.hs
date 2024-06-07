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
cpsExp e@(IntExp _) k i =
  let (_, ni) = (gensym i)
   in (AppExp k e, ni)
cpsExp e@(VarExp _) k i =
  let (_, ni) = (gensym i)
   in (AppExp k e, ni)
--- #### Define `cpsExp` for Application Expressions
cpsExp (AppExp f e) k i =
  case (isSimple e) of
    True ->
      let (_, ni) = (gensym i)
       in (AppExp k (AppExp f e), ni)
    False ->
      let (v, ni) = (gensym i)
       in let (v2, ni2) = (gensym ni)
           in ( ( AppExp
                    k
                    ( AppExp
                        e
                        ( LamExp
                            v
                            ( AppExp
                                f
                                (VarExp v)
                            )
                        )
                    )
                ),
                ni
              )
cpsExp _ _ _ = undefined

--- #### Define `cpsExp` for Operator Expressions

--- #### Define `cpsExp` for If Expressions

--- ### Define `cpsDecl`

cpsDecl :: Stmt -> Stmt
cpsDecl = undefined
