module Printer
  ( modName
  , showExpression
  , startAlways
  , endAlways
  ) where

import System.IO

import Operators
import Ranged
import SymbolTable
import VInt

import qualified Parser as P
import qualified Expressions as E
import qualified Width as W

-- TODO: This code doesn't know about associativity, so you get
--       slightly more parentheses than you might want.

modName :: W.Module -> String
modName = P.symName . rangedData . W.modName

symName :: SymbolTable a -> Symbol -> String
symName st sym = P.symName $ rangedData $ stNameAt sym st

parenthesize :: Int -> String -> Int -> String
parenthesize this str that = if this <= that then "(" ++ str ++ ")" else str

showExprRec :: SymbolTable (Ranged E.Slice) -> Int -> Ranged E.Expression -> String
showExprRec syms that rexpr =
  let (this, str) = showExpression' syms (rangedData rexpr) in
    parenthesize this str that

showExpression' :: SymbolTable (Ranged E.Slice) -> E.Expression -> (Int, String)

showExpression' syms (E.ExprSym sym) = (100, symName syms sym)

showExpression' _ (E.ExprInt vint) = (100, printVInt vint)

showExpression' syms (E.ExprSel rsym ra rb) =
  (100, symName syms (rangedData rsym) ++ "[" ++
        se ra ++ (case rb of Nothing -> "]" ; Just rb' -> ":" ++ se rb' ++ "]"))
  where se = showExprRec syms 1

showExpression' syms (E.ExprConcat re0 res) =
  (100,
   "{" ++ se re0 ++ concatMap (\ re -> ", " ++ se re) res ++ "}")
  where se = showExprRec syms 14

showExpression' syms (E.ExprReplicate n re) =
  (100, "{" ++ show n ++ "{" ++ showExprRec syms 13 re ++ "}}")

showExpression' syms (E.ExprUnOp ruo re) =
  (prec, showUnOp uo ++ " " ++ showExprRec syms prec re)
  where uo = rangedData ruo
        prec = unOpPrecedence uo

showExpression' syms (E.ExprBinOp rbo ra rb) =
  (prec, se ra ++ " " ++ showBinOp bo ++ " " ++ se rb)
  where bo = rangedData rbo
        prec = binOpPrecedence bo
        se = showExprRec syms prec

showExpression' syms (E.ExprCond ra rb rc) =
  (1, se ra ++ " ? " ++ se rb ++ " : " ++ se rc)
  where se = showExprRec syms 1

showExpression :: SymbolTable (Ranged E.Slice) -> E.Expression -> String
showExpression syms expr = snd $ showExpression' syms expr

startAlways :: Handle -> IO ()
startAlways handle =
  put "  always @(posedge clk or negedge rst_n) begin\n" >>
  put "    if (rst_n) begin\n"
  where put = hPutStr handle

endAlways :: Handle -> IO ()
endAlways h =
  put "    end\n" >>
  put "  end\n"
  where put = hPutStr h
