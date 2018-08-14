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

import Ranged
import SymbolTable
import Printer

import qualified DPI
import qualified SV

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

-- Print the start and end of a module, wrapping a body printing
-- function inside.
printModule :: Int -> Int -> W.Module -> Handle -> IO ()
printModule hash modIdx mod h =
  do { print fileHeader
     ; print start
     ; print (head portStrs)
     ; mapM_ (\ str -> print indent >> print str) (tail portStrs)
     ; print ");\n\n"
     ; print "`ifndef ACOV_SV\n"
     ; DPI.printModule h hash modIdx mod
     ; print "`else\n"
     ; SV.printModule h mod
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

dumpModule :: FilePath -> Int -> (Int, W.Module) -> IO ()
dumpModule dirname hash (modIdx, mod) =
  withFile (dirname </> (modName mod ++ "_coverage.sv")) WriteMode
  (printModule hash modIdx mod)

run :: FilePath -> (Int, [W.Module]) -> IO ()
run dirname (hash, mods) = createDirectoryIfMissing False dirname >>
                           mapM_ (dumpModule dirname hash) (zip [0..] mods)
