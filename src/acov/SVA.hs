module SVA
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
type RecSyms = SymbolTable ()
type GrpBody = Either (SymbolTable (), [W.Record]) W.BitsRecord

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
writeGroup h syms (idx, W.Group guards grpBody) =
  prepGroup h syms idx grpBody >>
  put ("  covergroup " ++ name ++ " " ++
       showGuards syms guards ++ ";\n") >>
  writeRecs h syms idx grpBody >>
  put "  endgroup\n\n" >>
  put ("  " ++ name ++ " " ++ name ++ "_Inst = new;\n")
  where put = hPutStr h
        name = "group_" ++ show idx

showSlice :: Int -> String
showSlice w = if w == 1 then ""
              else "[" ++ show (w - 1) ++ ":0] "

prepGroup :: Handle -> PortSyms -> Int -> GrpBody -> IO ()

prepGroup h syms idx (Left (recSyms, recs)) =
  mapM_ (writeRecWire h syms idx recSyms) recs

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

writeRecs h syms idx (Left (recSyms, recs)) =
  mapM_ (writeRecCP h syms idx recSyms) recs >>
  if not $ null $ tail recs then
    hPutStr h ("    cross " ++
               intercalate ", " (map (recName recSyms) recs) ++ ";\n")
  else
    return ()

writeRecs h syms idx (Right brec) = mapM_ f [0..(width - 1)]
  where f i = hPutStr h $
              "    " ++ name ++ "_" ++ show i ++
              ": coverpoint " ++ wireBase ++ "_bit_" ++ show i ++ ";\n"
        width = W.brWidth brec
        name = P.symName $ rangedData $ W.brSym brec
        wireBase = "acov_grp_" ++ show idx

recName :: RecSyms -> W.Record -> String
recName syms record = symName syms (rangedData (W.recSym record))

wireName :: Int -> RecSyms -> W.Record -> String
wireName grpIdx syms record =
  "acov_grp_" ++ show grpIdx ++ "_" ++ recName syms record

writeRecWire :: Handle -> PortSyms -> Int -> RecSyms -> W.Record -> IO ()
writeRecWire h pST grpIdx rST record =
  hPutStr h ("  wire " ++ showSlice width ++ name ++ " = " ++ value ++ ";\n")
  where width = W.recWidth record
        name = wireName grpIdx rST record
        value = showExpression pST (rangedData (W.recExpr record))

writeRecCP :: Handle -> PortSyms -> Int -> RecSyms -> W.Record -> IO ()
writeRecCP h pST grpIdx rST record =
  put ("    " ++ recname ++ ": coverpoint " ++ wirename ++ " {\n") >>
  put ("      bins x[] = {" ++ bins ++ "};\n") >>
  put ("    }\n")
  where put = hPutStr h
        recname = recName rST record
        wirename = wireName grpIdx rST record
        name = wireName grpIdx rST record
        bins = intercalate ", " $ map showIVL $ rlIntervals $ W.recClist record
        showIVL (lo, hi) = if lo == hi then show lo
                           else "[" ++ show lo ++ ":" ++ show hi ++ "]"
