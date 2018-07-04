module Operators
  ( UnOp(..)
  , BinOp(..)
  ) where

data UnOp = LogNot | BitNot
          | RedAnd | RedOr | RedNand | RedNor | RedXor | RedXNor
          | UPlus | UMinus
  deriving Show

data BinOp = Times | Divide | Modulo
           | Plus | Minus
           | LShift | RShift
           | Greater | GreaterEq | Less | LessEq
           | LogEq | LogNEq
           | CaseEq | CaseNEq
           | BitAnd
           | BitXor | BitXNor
           | BitOr
           | LogAnd
           | LogOr
  deriving Show
