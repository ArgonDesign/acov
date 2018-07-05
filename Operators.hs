module Operators
  ( UnOp(..)
  , unOpIsReduction
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

unOpIsReduction :: UnOp -> Bool
unOpIsReduction RedAnd = True
unOpIsReduction RedOr = True
unOpIsReduction RedNand = True
unOpIsReduction RedNor = True
unOpIsReduction RedXor = True
unOpIsReduction RedXnor = True
unOpIsReduction _ = False
