module DPI
  ( printModule
  ) where

import Control.Exception.Base
import Data.List
import System.IO

import Numeric (showHex)

import Printer
import Ranged
import SymbolTable

import qualified Width as W
import qualified Expressions as E

printModule :: Handle -> Int -> Int -> W.Module -> IO ()
printModule h hash modIdx mod =
  do { hPutStr h (imports hash)
     ; grps <- mapM (writeWire h syms) (zip [0..] (W.modGroups mod))
     ; startAlways h
     ; mapM_ (writeGroup h modIdx syms) (zip [0..] grps)
     ; endAlways h
     }
  where syms = W.modSyms mod

imports :: Int -> String
imports hash =
  unlines [ "  import \"DPI-C\" context acov_record1 ="
          , "    function void acov_record1 (input longint mod,"
          , "                                input byte cover_bits,"
          , "                                input longint grp,"
          , "                                input longint val);"
          , "  import \"DPI-C\" context acov_record2 ="
          , "    function void acov_record2 (input longint mod,"
          , "                                input byte cover_bits,"
          , "                                input longint grp,"
          , "                                input longint val1,"
          , "                                input longint val0);"
          , "  import \"DPI-C\" context acov_record3 ="
          , "    function void acov_record3 (input longint mod,"
          , "                                input byte cover_bits,"
          , "                                input longint grp,"
          , "                                input longint val2,"
          , "                                input longint val1,"
          , "                                input longint val0);"
          , "  import \"DPI-C\" context acov_record4 ="
          , "    function void acov_record4 (input longint mod,"
          , "                                input byte cover_bits,"
          , "                                input longint grp,"
          , "                                input longint val3,"
          , "                                input longint val2,"
          , "                                input longint val1,"
          , "                                input longint val0);"
          , ""
          , "  import \"DPI-C\" function void acov_open (input longint hash);"
          , "  import \"DPI-C\" function void acov_close ();"
          , ""
          , "  initial acov_open (64'h" ++ showHex hash "" ++ ");"
          , "  final acov_close ();"
          , ""
          ]

writeWire :: Handle -> SymbolTable (Ranged E.Slice) -> (Int, W.Group) ->
             IO ([Ranged E.Expression], Bool, Int)
writeWire handle syms (idx, grp) =
  assert (width > 0)
  put "  wire [" >>
  put (show $ width - 1) >>
  put ":0] " >>
  put name >>
  put ";\n  assign " >>
  put name >>
  put " = " >>
  put (showExpression syms (E.ExprConcat (head exprs) (tail exprs))) >>
  put ";\n\n" >>
  return (W.grpGuards grp, covBits, width)
  where put = hPutStr handle
        name = "acov_recgroup_" ++ show idx
        width = W.grpWidth grp
        exprs = W.grpExprs grp
        covBits = W.grpIsCovBits grp

boolByte :: Bool -> String
boolByte False = "8'b0"
boolByte True = "8'b1"

writeGroup :: Handle -> Int -> SymbolTable (Ranged E.Slice) ->
              (Int, ([Ranged E.Expression], Bool, Int)) -> IO ()
writeGroup handle modIdx syms (idx, (guards, isCovBits, width)) =
  -- TODO: We need a pass to guarantee the assertions hold
  assert (nwords > 0)
  assert (nwords <= 4) $
  do { guarded <- startGuard handle syms guards
     ; put $ (if guarded then "  " else "") ++ "      acov_record"
     ; put $ show nwords
     ; put (" (" ++ show modIdx ++ ", " ++
            boolByte isCovBits ++ ", " ++ show idx ++ ", ")
     ; put $ showRecArgs idx width
     ; put ");\n"
     ; endGuard handle guarded
     }
  where put = hPutStr handle
        nwords = quot (width + 63) 64

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

startGuard :: Handle -> SymbolTable (Ranged E.Slice) ->
              [Ranged E.Expression] -> IO Bool
startGuard _ _ [] = return False
startGuard handle syms guards =
  put "      if ((" >>
  put (intercalate ") && (" (map (showExpression syms . rangedData) guards)) >>
  put ")) begin\n" >>
  return True
  where put = hPutStr handle

endGuard :: Handle -> Bool -> IO ()
endGuard _ False = return ()
endGuard handle True = hPutStr handle "      end\n"
