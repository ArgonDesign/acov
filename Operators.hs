module Operators
  ( UnOp(..)
  , BinOp(..)
  ) where

data UnOp = LogNot | BitNot
          | RedAnd | RedOr | RedNand | RedNor | RedXor | RedXnor
          | UPlus | UMinus
  deriving Show

data BinOp = Times | Divide | Modulo
           | Plus | Minus
           | LShift | RShift
           | Greater | GreaterEq | Less | LessEq
           | LogEq | LogNeq
           | CaseEq | CaseNeq
           | BitAnd
           | BitXor | BitXnor
           | BitOr
           | LogAnd
           | LogOr
  deriving Show
