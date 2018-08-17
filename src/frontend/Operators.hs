module Operators
  ( UnOp(..)
  , unOpIsReduction
  , unOpPrecedence
  , showUnOp
  , BinOp(..)
  , binOpIsReduction
  , binOpPrecedence
  , showBinOp
  ) where

import Hashable

data UnOp = LogNot | BitNot
          | RedAnd | RedOr | RedNand | RedNor | RedXor | RedXnor
          | UPlus | UMinus

instance Hashable UnOp where
  hash LogNot = 0
  hash BitNot = 1
  hash RedAnd = 2
  hash RedOr = 3
  hash RedNand = 4
  hash RedNor = 5
  hash RedXor = 6
  hash RedXnor = 7
  hash UPlus = 8
  hash UMinus = 9

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

instance Hashable BinOp where
  hash Times = 0
  hash Divide = 1
  hash Modulo = 2
  hash Plus = 3
  hash Minus = 4
  hash LShift = 5
  hash RShift = 6
  hash Greater = 7
  hash GreaterEq = 8
  hash Less = 9
  hash LessEq = 10
  hash LogEq = 11
  hash LogNeq = 12
  hash CaseEq = 13
  hash CaseNeq = 14
  hash BitAnd = 15
  hash BitXor = 16
  hash BitXnor = 17
  hash BitOr = 18
  hash LogAnd = 19
  hash LogOr = 20

unOpIsReduction :: UnOp -> Bool
unOpIsReduction RedAnd = True
unOpIsReduction RedOr = True
unOpIsReduction RedNand = True
unOpIsReduction RedNor = True
unOpIsReduction RedXor = True
unOpIsReduction RedXnor = True
unOpIsReduction _ = False

unOpPrecedence :: UnOp -> Int
unOpPrecedence UPlus = 15
unOpPrecedence UMinus = 15
unOpPrecedence _ = 16

showUnOp :: UnOp -> String
showUnOp LogNot = "!"
showUnOp BitNot = "~"
showUnOp RedAnd = "&"
showUnOp RedOr = "|"
showUnOp RedNand = "~&"
showUnOp RedNor = "~|"
showUnOp RedXor = "^"
showUnOp RedXnor = "~^"
showUnOp UPlus = "+"
showUnOp UMinus = "-"

binOpIsReduction :: BinOp -> Bool
binOpIsReduction Greater = True
binOpIsReduction GreaterEq = True
binOpIsReduction Less = True
binOpIsReduction LessEq = True
binOpIsReduction LogEq = True
binOpIsReduction LogNeq = True
binOpIsReduction LogAnd = True
binOpIsReduction LogOr = True
binOpIsReduction _ = False

binOpPrecedence :: BinOp -> Int
binOpPrecedence Times = 12
binOpPrecedence Divide = 12
binOpPrecedence Modulo = 12
binOpPrecedence Plus = 11
binOpPrecedence Minus = 11
binOpPrecedence LShift = 10
binOpPrecedence RShift = 10
binOpPrecedence Greater = 9
binOpPrecedence GreaterEq = 9
binOpPrecedence Less = 9
binOpPrecedence LessEq = 9
binOpPrecedence LogEq = 8
binOpPrecedence LogNeq = 8
binOpPrecedence CaseEq = 7
binOpPrecedence CaseNeq = 7
binOpPrecedence BitAnd = 6
binOpPrecedence BitXor = 5
binOpPrecedence BitXnor = 5
binOpPrecedence BitOr = 4
binOpPrecedence LogAnd = 3
binOpPrecedence LogOr = 2

showBinOp :: BinOp -> String
showBinOp Times = "*"
showBinOp Divide = "/"
showBinOp Modulo = "%"
showBinOp Plus = "+"
showBinOp Minus = "-"
showBinOp LShift = "<<"
showBinOp RShift = ">>"
showBinOp Greater = ">"
showBinOp GreaterEq = ">="
showBinOp Less = "<"
showBinOp LessEq = "<="
showBinOp LogEq = "=="
showBinOp LogNeq = "!="
showBinOp CaseEq = "==="
showBinOp CaseNeq = "!=="
showBinOp BitAnd = "&"
showBinOp BitXor = "^"
showBinOp BitXnor = "~^"
showBinOp BitOr = "|"
showBinOp LogAnd = "&&"
showBinOp LogOr = "||"
