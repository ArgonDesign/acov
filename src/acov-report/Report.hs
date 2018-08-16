module Report
  ( report
  ) where

import Control.Exception.Base
import qualified Data.IntSet as IS
import Data.List
import System.IO

import Count
import Ranged
import Time (stringTime)

import Parser (symName)
import qualified Width as W
import CountPass

import Numeric (showHex)

report :: Handle -> Coverage -> IO [FilePath]
report h cov =
  do { time <- stringTime
     ; put ("<html><head><title>Coverage report</title>\
            \<link rel='stylesheet' type='text/css' href='acov.css'/>\
            \</head><body>\
            \<section class='summary'><h1>Coverage report</h1>\
            \<p>Coverage based on " ++
            show (covTests cov) ++
            " tests; report generated at " ++ time ++ "</p><p>")
     ; put (showCounts "modules" (covCounts cov))
     ; put ("</p></section><section class=\"modules\">")
     ; mapM_ (reportMod h) (covMods cov)
     ; put "</section></body></html>"
     ; return ["data/acov.css"]
     }
  where put = hPutStr h
        mods = covMods cov

reportMod :: Handle -> ModCoverage -> IO ()
reportMod h mc =
  do { put $ "<section class=\"module\"><h1>Module " ++ (mcName mc) ++ "</h1>"
     ; if null scopes then
         put "<p>No scopes seen.</p>"
       else
         let singleScope = null (tail scopes) in
           do { if not singleScope then
                  put (showCounts "scopes" (mcCounts mc))
                else
                  return ()
              ; mapM_ (reportScope h singleScope) scopes
              }
     ; put "</section>"
     }
  where put = hPutStr h
        scopes = mcScopes mc

reportScope :: Handle -> Bool -> ScopeCoverage -> IO ()
reportScope h singleScope sc =
  do { put "<section class=\"scope\">"
     ; if not singleScope then put ("<h3>" ++ (scName sc) ++ "</h3>")
       else return ()
     ; put (showCounts "groups" (scCounts sc))
     ; mapM_ (reportGrp h) (scGroups sc)
     ; put "</section>"
     }
  where put = hPutStr h

-- Report missed items for a cross record
reportMisses :: Handle -> Count -> [W.Record] -> [[Integer]] -> IO ()
reportMisses h count recs missing =
  assert (not $ null $ missing) $
  put "<table class='missed-recs'><thead><tr>" >>
  mapM_ th recs >>
  put "</tr></thead><tbody>" >>
  mapM_ tr missing >>
  foot >>
  put "</tbody></table>"
  where put = hPutStr h
        nhits = length missing
        nfields = length (head missing)
        th r = put $ "<th>" ++ (symName $ rangedData $ W.recSym r) ++ "</th>"
        tr miss = put "<tr>" >> mapM_ td miss >> put "</tr>"
        td val = put $ "<td>" ++ show val ++ "</td>"
        foot = if countMissed count <= toInteger nhits
               then return ()
               else put "<tr class='foot'>" >>
                    mapM_ dotsTD [1..nfields] >>
                    put "</tr>"
        dotsTD _ = put "<td>...</td>"

-- Report missed bits for a "cover bits" record
reportBits :: Handle -> BitsCov -> BitsCov -> IO ()
reportBits h bc0 bc1 =
  do { put "<table class='missed-bits'>"
     ; reportBitsLine h "Zeros" bc0
     ; reportBitsLine h "Ones" bc1
     ; put "</table>"
     }
  where put = hPutStr h

reportBitsLine :: Handle -> String -> BitsCov -> IO ()
reportBitsLine h name (BitsCov w hits partial misses) =
  do { put ("<tr class='" ++ rowclass ++ "'><td>" ++
            name ++ " mask</td><td class='bitmask'>0x")
     ; reportHex h w hits
     ; put $ "</td><td>"
     ; if null misses then
         return ()
       else
         put "missing bits: " >>
         rptMiss True (head misses) >>
         mapM_ (rptMiss False) (tail misses) >>
         put (if partial then ", ..." else "")
     ; put "</td></tr>"
     }
  where put = hPutStr h
        rowclass = (if null misses then "full" else "partial") ++ "mask"
        rptMiss first bit = put $
                            (if first then "" else ", ") ++
                            show bit

reportHex :: Handle -> Int -> IS.IntSet -> IO ()
reportHex h w hits = mapM_ reportHC (reverse [0..(nchars - 1)])
  where nchars = quot (w + 3) 4
        reportHC k = hPutStr h $ showHex (mask $ 4 * k) ""
        mask bit0 = (ifbit 1 (0 + bit0)) +
                    (ifbit 2 (1 + bit0)) +
                    (ifbit 4 (2 + bit0)) +
                    (ifbit 8 (3 + bit0))
        ifbit n b = if IS.member b hits then n else 0

reportGrp :: Handle -> (String, GroupCoverage) -> IO ()
reportGrp h (name, cov) =
  put "<section class=\"group\">" >>
  put (grpHeader name cov) >>
  reportGrpBody h cov >>
  put "</section>"
  where put = hPutStr h

grpHeader :: String -> GroupCoverage -> String
grpHeader name gc = "<h4>" ++ name ++ " (" ++ fill (covCount gc) ++ ")</h4>"
  where fill Nothing = "ignored as scope doesn't match"
        fill (Just count) = showCount count

reportGrpBody :: Handle -> GroupCoverage -> IO ()

reportGrpBody h (RecCov count recs missing) =
  if countFull count then
    return ()
  else
    reportMisses h count recs missing

reportGrpBody h (BRecCov count bc0 bc1) =
  if countFull count then
    return ()
  else
    reportBits h bc0 bc1

reportGrpBody h BadScope = return ()
