module Syntax where

import           Lexer

type FileInfo = AlexPosn

type Name = String

data TermNode = TermNode
  { getFI :: FileInfo,
    getTm :: Term
  }
  deriving (Eq)

data Term
  = TmSComb
  | TmKComb
  | TmIComb
  | TmVar Name
  | TmApp TermNode TermNode
  deriving (Eq)

instance Show (TermNode) where
  show (TermNode _ tm) = show tm

instance Show (Term) where
  show TmSComb       = "S"
  show TmKComb       = "K"
  show TmIComb       = "I"
  show (TmVar x)     = x
  show (TmApp t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"
