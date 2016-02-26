module Hw05 where

import Data.Char
import Lexer5.x
import Parser.y

type Id = String

data Expr =
    EVar Id
  | ETrue
  | EFalse
  | EIf Expr Expr Expr
  | ENum Int
  | EIncr Expr
  | EDecr Expr
  | EIsZero Expr
  | EApp Expr Expr
  | ELam Id Expr
  deriving (Show, Eq)

data Token =
    TVar Id
  | TTrue
  | TFalse
  | TIf
  | TNum Int
  | TIncr
  | TDecr
  | TIsZero
  | TApp
  | TLam
  deriving (Show, Eq)

lexer :: String -> [Token]
lexer = alexScanTokens