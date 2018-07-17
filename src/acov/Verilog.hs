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
import SymbolTable

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

showPorts :: [(Ranged P.Symbol, Ranged E.Slice)] -> [String]
showPorts entries = map draw $ zip names sels
  where names = [P.Symbol "clk", P.Symbol "rst_n"] ++
                map (rangedData . fst) entries
        sels = showBitSels $ ([slice0, slice0] ++
                              map (rangedData . snd) entries)
        slice0 = E.Slice 0 0
        draw (sym, sel) = "input wire " ++ sel ++ " " ++ P.symName sym

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
  unlines [ "  import \"DPI-C\" context acov_record ="
          , "    function void acov_record (input string name, input longint value);"
          , "  import \"DPI-C\" function void acov_close ();"
          , ""
          , "`ifndef NO_FINAL"
          , "  final acov_close ();"
          , "`endif"
          , ""
          ]

beginModule :: Handle -> P.Symbol -> SymbolTable (Ranged E.Slice) -> IO ()
beginModule handle sym ports =
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
        portStrs = showPorts $ stAssocs ports

-- TODO: This doesn't know about associativity.
parenthesize :: Int -> String -> Int -> String
parenthesize this str that = if this <= that then "(" ++ str ++ ")" else str

symName :: SymbolTable a -> Symbol -> String
symName st sym = P.symName $ rangedData $ stNameAt sym st

showExpression :: SymbolTable (Ranged E.Slice) -> Int -> Ranged E.Expression -> String
showExpression syms that rexpr =
  let (this, str) = showExpression' syms (rangedData rexpr) in
    parenthesize this str that

showExpression' :: SymbolTable (Ranged E.Slice) -> E.Expression -> (Int, String)

showExpression' syms (E.ExprSym sym) = (100, symName syms sym)

showExpression' _ (E.ExprInt vint) = (100, printVInt vint)

showExpression' syms (E.ExprSel rsym ra rb) =
  (100, symName syms (rangedData rsym) ++ "[" ++
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

showExpression64 :: Int -> SymbolTable (Ranged E.Slice) ->
                    Ranged E.Expression -> String
showExpression64 w syms rexpr =
  if w /= 64 then "{" ++ show (64 - w) ++ "'b0, " ++ rest ++ "}"
  else rest
  where rest = showExpression syms 0 rexpr

startAlways :: Handle -> IO ()
startAlways handle =
  put "  always @(posedge clk or negedge rst_n) begin\n" >>
  put "    if (rst_n) begin\n"
  where put = hPutStr handle 

startGuard :: Handle -> SymbolTable (Ranged E.Slice) ->
              Maybe (Ranged E.Expression) -> IO Bool
startGuard _ _ Nothing = return False
startGuard handle syms (Just guard) =
  hPutStr handle "if (" >>
  hPutStr handle (showExpression syms 100 guard) >>
  hPutStr handle ") begin\n" >>
  return True

writeRecord :: Handle -> P.Symbol -> SymbolTable (Ranged E.Slice) ->
               SymbolTable Int -> Bool -> E.Record -> IO ()
writeRecord handle modname syms recs guarded (E.Record expr recsym) =
   put (if guarded then "  " else "" ++ "      ") >>
   put "acov_record" >>
   put " (\"" >>
   put (P.symName modname) >>
   put "." >>
   put (symName recs recsym) >>
   put "\", " >>
   put (showExpression64 width syms expr) >>
   put ");\n"
   where put = hPutStr handle
         width = stAt recsym recs

endGuard :: Handle -> Bool -> IO ()
endGuard _ False = return ()
endGuard handle True = hPutStr handle "      end\n"


writeBlock :: Handle -> P.Symbol -> SymbolTable (Ranged E.Slice) ->
              SymbolTable Int -> E.Block -> IO ()
writeBlock handle modname symST recST (E.Block guard recs) =
  do { guarded <- startGuard handle symST guard
     ; mapM_ (writeRecord handle modname symST recST guarded) recs
     ; endGuard handle guarded
     }

endModule :: Handle -> IO ()
endModule handle = put "    end\n  end\nendmodule\n\n" >> put fileFooter
  where put = hPutStr handle

writeModule :: Handle -> P.Symbol -> W.Module -> IO ()
writeModule handle name mod =
  beginModule handle name (W.modSyms mod) >>
  startAlways handle >>
  mapM_ (writeBlock handle name (W.modSyms mod) (W.modRecs mod))
  (W.modBlocks mod) >>
  endModule handle


dumpModule :: FilePath -> P.Symbol -> W.Module -> IO ()
dumpModule dirname name mod =
  withFile (dirname </> (P.symName name ++ "_coverage.v")) WriteMode
  (\ h -> writeModule h name mod)

dumpModules :: FilePath -> SymbolTable W.Module -> IO ()
dumpModules dirname mods = stTraverseWithSym_ f mods
  where f modname mod = dumpModule dirname (rangedData modname) mod

run :: FilePath -> W.Script -> IO ()
run dirname scr =
  createDirectoryIfMissing False dirname >>
  dumpModules dirname (W.scrModules scr)
