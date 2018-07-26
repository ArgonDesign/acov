module Verilog
  ( run
  ) where

import Control.Exception.Base
import Data.Array
import Data.Bits
import qualified Data.Foldable as Foldable
import Data.Functor
import Data.List
import System.Directory
import System.FilePath
import System.IO

import Operators
import Ranged
import VInt
import SymbolTable

import qualified Parser as P
import qualified Expressions as E
import qualified Width as W

fileHeader :: String
fileHeader =
  unlines [ "// AUTO-GENERATED FILE: Do not edit."
          , ""
          , "`default_nettype none"
          , ""
          ]

fileFooter :: String
fileFooter = "\n`default_nettype wire\n"


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

beginModule :: Handle -> String -> SymbolTable (Ranged E.Slice) -> IO ()
beginModule handle name ports =
  print imports
  where print = hPutStr handle
        start = "module " ++ name ++ "_coverage ("
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

writeWire :: Handle -> SymbolTable (Ranged E.Slice) -> (Int, W.Group) ->
             IO ([Ranged E.Expression], Int)
writeWire handle syms (idx, grp) =
  assert (width > 0)
  put "  wire [" >>
  put (show $ width - 1) >>
  put ":0] " >>
  put name >>
  put ";\n  assign " >>
  put name >>
  put " = " >>
  put (snd $ showExpression' syms (E.ExprConcat (head exprs) (tail exprs))) >>
  put ";\n\n" >>
  return (W.grpGuards grp, width)
  where put = hPutStr handle
        name = "acov_recgroup_" ++ show idx
        width = W.grpWidth grp
        exprs = W.grpExprs grp

startGuard :: Handle -> SymbolTable (Ranged E.Slice) ->
              [Ranged E.Expression] -> IO Bool
startGuard _ _ [] = return False
startGuard handle syms guards =
  put "      if ((" >>
  put (intercalate ") && (" (map (showExpression syms 0) guards)) >>
  put ")) begin\n" >>
  return True
  where put = hPutStr handle

endGuard :: Handle -> Bool -> IO ()
endGuard _ False = return ()
endGuard handle True = hPutStr handle "      end\n"

showRecArgs :: Int -> Int -> String
showRecArgs idx width =
  assert (width > 0)
  "{" ++
  (if pad >= 0 then
     show pad ++ "'b0, " ++ slice (width - 1) (width + pad - 64)
   else
     "") ++
  rst False (quot width 64) ++ "}"
  where pad = 63 - rem (width + 63) 64
        name = "acov_recgroup_" ++ show idx
        slice top bot =
          name ++ "[" ++
          (if top == bot then show top else show top ++ ":" ++ show bot)
          ++ "]"
        rst _ 0 = ""
        rst comma nleft =
          (if comma then ", " else "") ++
          slice (64 * nleft - 1) (64 * (nleft - 1)) ++
          rst True (nleft - 1)

writeGroup :: Handle -> String -> SymbolTable (Ranged E.Slice) ->
              (Int, ([Ranged E.Expression], Int)) -> IO ()
writeGroup handle modname syms (idx, (guard, width)) =
  -- TODO: We need a pass to guarantee the assertions hold
  assert (nwords > 0)
  assert (nwords <= 4) $
  do { guarded <- startGuard handle syms guard
     ; put $ (if guarded then "  " else "") ++ "      acov_record"
     ; put $ show nwords
     ; put $ " (\"" ++ modname ++ "\", " ++ show idx ++ ", "
     ; put $ showRecArgs idx width
     ; put ");\n"
     ; endGuard handle guarded
     }
  where put = hPutStr handle
        nwords = quot (width + 63) 64

imports :: String
imports =
  unlines [ "  import \"DPI-C\" context acov_record1 ="
          , "    function void acov_record1 (input string mod,"
          , "                                input longint grp,"
          , "                                input longint val);"
          , "  import \"DPI-C\" context acov_record2 ="
          , "    function void acov_record2 (input string mod,"
          , "                                input longint grp,"
          , "                                input longint val1,"
          , "                                input longint val0);"
          , "  import \"DPI-C\" context acov_record3 ="
          , "    function void acov_record3 (input string mod,"
          , "                                input longint grp,"
          , "                                input longint val2,"
          , "                                input longint val1,"
          , "                                input longint val0);"
          , "  import \"DPI-C\" context acov_record4 ="
          , "    function void acov_record4 (input string mod,"
          , "                                input longint grp,"
          , "                                input longint val3,"
          , "                                input longint val2,"
          , "                                input longint val1,"
          , "                                input longint val0);"
          , ""
          , "  import \"DPI-C\" function void acov_close ();"
          , ""
          , "  final acov_close ();"
          , ""
          ]

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

printDPI :: Handle -> W.Module -> IO ()
printDPI h mod =
  do { hPutStr h imports
     ; grps <- mapM (writeWire h syms) (zip [0..] (W.modGroups mod))
     ; startAlways h
     ; mapM_ (writeGroup h name syms) (zip [0..] grps)
     ; endAlways h
     }
  where syms = W.modSyms mod
        name = modName mod

printSVA :: Handle -> W.Module -> IO ()
printSVA h _ =
  hPutStr h "  // SVA backend not yet implemented.\n"

modName :: W.Module -> String
modName = P.symName . rangedData . W.modName

-- Print the start and end of a module, wrapping a body printing
-- function inside.
printModule :: W.Module -> Handle -> IO ()
printModule mod h =
  do { print fileHeader
     ; print start
     ; print (head portStrs)
     ; mapM_ (\ str -> print indent >> print str) (tail portStrs)
     ; print ");\n\n"
     ; print "`ifdef ACOV_SVA\n"
     ; printSVA h mod
     ; print "`else\n"
     ; printDPI h mod
     ; print "`endif\n"
     ; print "endmodule\n\n"
     ; print fileFooter
     }
  where print = hPutStr h
        name = modName mod
        start = "module " ++ name ++ "_coverage ("
        indent = ",\n" ++ replicate (length start) ' '
        ports = W.modSyms mod
        portStrs = showPorts $ stAssocs ports

dumpModule :: FilePath -> W.Module -> IO ()
dumpModule dirname mod =
  withFile (dirname </> (modName mod ++ "_coverage.v")) WriteMode
  (printModule mod)

run :: FilePath -> [W.Module] -> IO ()
run dirname mods = createDirectoryIfMissing False dirname >>
                   mapM_ (dumpModule dirname) mods
