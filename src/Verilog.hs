module Verilog
  ( run
  ) where

import Control.Exception.Base
import Data.Array
import Data.Bits
import Data.Functor
import Data.Maybe
import System.Directory
import System.FilePath
import System.IO

import Operators
import Ranged
import VInt

import qualified Parser as P
import qualified Symbols as S
import qualified Expressions as E
import qualified Width as W

showBitSel :: E.Slice -> String
showBitSel (E.Slice a b) =
  if a == 0 && b == 0 then ""
  else "[" ++ show a ++ ":" ++ show b ++ "]"

showBitSels :: [E.Slice] -> [String]
showBitSels slices = map expand raw
  where raw = map showBitSel slices
        width = maximum $ map length raw
        expand s = assert (length s <= width) $
                   s ++ replicate (width - length s) ' '

showPorts :: [S.SymbolEntry (Ranged E.Slice)] -> [String]
showPorts entries =
  map draw (zip (map S.symbolEntrySymbol entries) sels)
  where getSlice = rangedData . S.symbolEntryData
        sels = showBitSels (map getSlice entries)
        draw (sym, sel) = "input wire " ++ sel ++ " " ++ P.symName sym

fakeRange :: LCRange
fakeRange = LCRange (LCPos 0 0) (LCPos 0 0)

defaultPorts :: [S.SymbolEntry (Ranged E.Slice)]
defaultPorts =
  [S.SymbolEntry (P.Symbol "clk") (Ranged fakeRange $ E.Slice 0 0),
   S.SymbolEntry (P.Symbol "rst_n") (Ranged fakeRange $ E.Slice 0 0)]

fileHeader :: String
fileHeader =
  unlines [ "// AUTO-GENERATED FILE: Do not edit."
          , ""
          , "`default_nettype none"
          , ""
          ]

fileFooter :: String
fileFooter = "`default_nettype wire\n"

imports :: String
imports =
  unlines [ "  import \"DPI-C\" function void acov_record (input string name,"
          , "                                            input longint value);"
          , "  import \"DPI-C\" function void acov_close ();"
          , ""
          , "`ifndef NO_FINAL"
          , "  final acov_close ();"
          , "`endif"
          , ""
          ]

beginModule :: Handle -> P.Symbol -> S.SymbolArray (Ranged E.Slice) -> IO ()
beginModule handle sym portArray =
  assert (not $ null portStrs) $
  print fileHeader >>
  print start >>
  print (head portStrs) >>
  mapM_ (\ str -> print indent >> print str) (tail portStrs) >>
  print ");\n\n" >>
  print imports
  where print = hPutStr handle
        start = "module " ++ P.symName sym ++ "_coverage ("
        indent = ",\n" ++ replicate (length start) ' '
        ports = S.symbolArrayElems portArray
        portStrs = showPorts $ defaultPorts ++ ports

-- TODO: This doesn't know about associativity.
parenthesize :: Int -> String -> Int -> String
parenthesize this str that = if this <= that then "(" ++ str ++ ")" else str

showExpression :: S.SymbolArray (Ranged E.Slice) -> Int -> Ranged E.Expression -> String
showExpression syms that rexpr =
  let (this, str) = showExpression' syms (rangedData rexpr) in
    parenthesize this str that

showExpression' :: S.SymbolArray (Ranged E.Slice) -> E.Expression -> (Int, String)

showExpression' syms (E.ExprSym sym) = (100, S.symbolName syms sym)

showExpression' _ (E.ExprInt vint) = (100, printVInt vint)

showExpression' syms (E.ExprSel rsym ra rb) =
  (100, S.symbolName syms (rangedData rsym) ++ "[" ++
        se ra ++ (case rb of Nothing -> "]" ; Just rb' -> ":" ++ se rb' ++ "]"))
  where se = showExpression syms 1

showExpression' syms (E.ExprConcat re0 res) =
  (100,
   "{" ++ se re0 ++ concatMap (\ re -> ", " ++ se re) res ++ "}")
  where se = showExpression syms 14

showExpression' syms (E.ExprReplicate n re) =
  (100, "{" ++ show n ++ "{" ++ showExpression syms 13 re ++ "}}")

showExpression' syms (E.ExprUnOp ruo re) =
  (prec, showUnOp uo ++ " " ++ showExpression syms prec re)
  where uo = rangedData ruo
        prec = unOpPrecedence uo

showExpression' syms (E.ExprBinOp rbo ra rb) =
  (prec, se ra ++ " " ++ showBinOp bo ++ " " ++ se rb)
  where bo = rangedData rbo
        prec = binOpPrecedence bo
        se = showExpression syms prec

showExpression' syms (E.ExprCond ra rb rc) =
  (1, se ra ++ " ? " ++ se rb ++ " : " ++ se rc)
  where se = showExpression syms 1

triggers :: [E.ModStmt] -> [(S.Symbol, Ranged E.Expression)]
triggers = mapMaybe unpackTrigger
  where unpackTrigger (E.Assign a b) = Just (rangedData a, b)
        unpackTrigger (E.Record _ _ _) = Nothing

records :: [E.ModStmt] -> [(Maybe S.Symbol, Ranged E.Expression, S.Symbol)]
records = mapMaybe unpackRecord
  where unpackRecord (E.Assign _ _) = Nothing
        unpackRecord (E.Record a b c) =
          Just (rangedData <$> a, b, rangedData c)

writeTrigger :: Handle -> S.SymbolArray (Ranged E.Slice) -> S.SymbolArray () ->
                S.Symbol -> Ranged E.Expression -> IO ()
writeTrigger handle ports triggers sym expr =
  put "  wire " >>
  put name >>
  put ";\n  assign " >>
  put name >>
  put " = " >>
  put (showExpression ports 0 expr) >>
  put ";\n\n"
  where put = hPutStr handle
        name = S.symbolName triggers sym

writeTriggers :: Handle -> S.SymbolArray (Ranged E.Slice) -> S.SymbolArray () -> 
                 [(S.Symbol, Ranged E.Expression)] -> IO ()
writeTriggers handle ports triggers =
  mapM_ (\ (s, re) -> writeTrigger handle ports triggers s re)

startAlways :: Handle -> IO ()
startAlways handle =
  put "  always @(posedge clk or negedge rst_n) begin\n" >>
  put "    if (rst_n) begin\n"
  where put = hPutStr handle 

writeGuard :: Handle -> S.SymbolArray () -> Maybe S.Symbol -> IO ()
writeGuard handle triggers Nothing = return ()
writeGuard handle triggers (Just sym) =
  hPutStr handle "if (" >>
  hPutStr handle (S.symbolName triggers sym) >>
  hPutStr handle ") "

writeRecord ::
  Handle ->
  S.SymbolArray (Ranged E.Slice) -> S.SymbolArray () -> S.SymbolArray Int ->
  Maybe S.Symbol -> Ranged E.Expression -> S.Symbol ->
  IO ()
writeRecord handle ports triggers records guard expr recname =
  put "      " >>
  writeGuard handle triggers guard >>
  put "acov_record" >>
  put " (\"" >>
  put (S.symbolName records recname) >>
  put "\", " >>
  put (showExpression ports 0 expr) >>
  put ");\n"
  where put = hPutStr handle
        width = S.symbolData records recname

writeRecords ::
  Handle ->
  S.SymbolArray (Ranged E.Slice) -> S.SymbolArray () -> S.SymbolArray Int ->
  [(Maybe S.Symbol, Ranged E.Expression, S.Symbol)] ->
  IO ()
writeRecords handle ports triggers records =
  mapM_ (\ (g, e, n) -> writeRecord handle ports triggers records g e n)

endBlocks :: Handle -> IO ()
endBlocks handle = put "    end\n  end\nendmodule\n\n" >> put fileFooter
  where put = hPutStr handle

writeModule :: Handle -> P.Symbol -> W.Module -> IO ()
writeModule handle name (W.Module mst stmts) =
  beginModule handle name pts >>
  writeTriggers handle pts tgs (triggers stmts) >>
  startAlways handle >>
  writeRecords handle pts tgs rcs (records stmts) >>
  endBlocks handle
  where pts = W.mstPorts mst
        tgs = W.mstTriggers mst
        rcs = W.mstRecords mst

dumpModule :: FilePath -> P.Symbol -> W.Module -> IO ()
dumpModule dirname name mod =
  withFile (dirname </> (P.symName name ++ "_coverage.v")) WriteMode
  (\ h -> writeModule h name mod)

dumpModules :: FilePath -> S.SymbolArray W.Module -> IO ()
dumpModules dirname (S.SymbolArray mods) = mapM_ doSE $ elems mods
  where doSE (S.SymbolEntry name mod) = dumpModule dirname name mod

run :: FilePath -> W.Script -> IO ()
run dirname scr =
  createDirectoryIfMissing False dirname >>
  dumpModules dirname (W.scrModules scr)
