module SV
  ( printModule
  ) where

import Control.Exception.Base
import Data.List
import System.IO

import Printer
import Ranged
import RangeList
import SymbolTable

import qualified Width as W
import qualified Expressions as E
import qualified Parser as P

type PortSyms = SymbolTable (Ranged E.Slice)
type GrpBody = Either [W.Record] W.BitsRecord

printModule :: Handle -> W.Module -> IO ()
printModule h mod = mapM_ (writeGroup h syms) (zip [0..] grps)
  where syms = W.modSyms mod
        grps = W.modGroups mod

showGuards :: PortSyms -> [Ranged E.Expression] -> String
showGuards _ [] = "@(posedge clk iff rst_n)"
showGuards syms guards =
  "@(posedge clk iff rst_n && (" ++
  intercalate ") && (" (map (showExpression syms . rangedData) guards) ++
  "))"

writeGroup :: Handle -> PortSyms -> (Int, W.Group) -> IO ()
writeGroup h syms (idx, grp) =
  prepGroup h syms idx (W.grpRecs grp) >>
  put ("  covergroup " ++ name ++ " " ++
       showGuards syms (W.grpGuards grp) ++ ";\n") >>
  writeRecs h syms idx (W.grpRecs grp) >>
  put "  endgroup\n\n" >>
  put ("  " ++ name ++ " " ++ name ++ "_Inst = new;\n")
  where put = hPutStr h
        name = "group_" ++ show idx

showSlice :: Int -> String
showSlice w = if w == 1 then ""
              else "[" ++ show (w - 1) ++ ":0] "

prepGroup :: Handle -> PortSyms -> Int -> GrpBody -> IO ()

prepGroup h syms idx (Left recs) =
  mapM_ (writeRecWire h syms idx) recs

prepGroup h syms idx (Right brec) =
  do { put ("  wire " ++ showSlice width ++ wireBase ++ " = " ++
            showExpression syms (rangedData (W.brExpr brec)) ++ ";\n")
     ; mapM_ showBit [0..(width - 1)]
     }
  where put = hPutStr h
        width = W.brWidth brec
        wireBase = "acov_grp_" ++ show idx
        showBit i = put ("  wire " ++ wireBase ++ "_bit_" ++ show i ++
                         " = " ++ wireBase ++ "[" ++ show i ++ "];\n")

writeRecs :: Handle -> PortSyms -> Int -> GrpBody -> IO ()

writeRecs h syms idx (Left recs) =
  mapM_ (writeRecCP h syms idx) recs >>
  if not $ null $ tail recs then
    hPutStr h ("    cross " ++
               intercalate ", " (map recName recs) ++ ";\n")
  else
    return ()

writeRecs h syms idx (Right brec) = mapM_ f [0..(width - 1)]
  where f i = hPutStr h $
              "    " ++ name ++ "_" ++ show i ++
              ": coverpoint " ++ wireBase ++ "_bit_" ++ show i ++ ";\n"
        width = W.brWidth brec
        name = P.symName $ rangedData $ W.brSym brec
        wireBase = "acov_grp_" ++ show idx

recName :: W.Record -> String
recName = P.symName . rangedData . W.recSym

wireName :: Int -> W.Record -> String
wireName grpIdx record =
  "acov_grp_" ++ show grpIdx ++ "_" ++ recName record

writeRecWire :: Handle -> PortSyms -> Int -> W.Record -> IO ()
writeRecWire h pST grpIdx record =
  hPutStr h ("  wire " ++ showSlice width ++ name ++ " = " ++ value ++ ";\n")
  where width = W.recWidth record
        name = wireName grpIdx record
        value = showExpression pST (rangedData (W.recExpr record))

writeRecCP :: Handle -> PortSyms -> Int -> W.Record -> IO ()
writeRecCP h pST grpIdx record =
  put ("    " ++ recname ++ ": coverpoint " ++ wirename ++ " {\n") >>
  put ("      bins x[] = {" ++ bins ++ "};\n") >>
  put ("    }\n")
  where put = hPutStr h
        recname = recName record
        wirename = wireName grpIdx record
        name = wireName grpIdx record
        bins = intercalate ", " $ map showIVL $ rlIntervals $ W.recClist record
        showIVL (lo, hi) = if lo == hi then show lo
                           else "[" ++ show lo ++ ":" ++ show hi ++ "]"
