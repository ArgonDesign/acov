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
            \<h1>Coverage report</h1>\
            \<p>Coverage based on " ++
            show (covTests cov) ++
            " tests; report generated at " ++ time ++ "</p><p>")
     ; put (showCounts "modules" (covCounts cov))
     ; put ("</p><section class=\"modules\"><h1>Modules</h1>")
     ; mapM_ (reportMod h) (covMods cov)
     ; put "</section></body></html>"
     ; return ["data/acov.css"]
     }
  where put = hPutStr h
        mods = covMods cov

reportMod :: Handle -> ModCoverage -> IO ()
reportMod h mc =
  do { put $ "<section class=\"module\"><h2>Module " ++ (mcName mc) ++ "</h2>"
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

showMiss :: [W.Record] -> [Integer] -> String
showMiss recs vals =
  assert (not $ null vals) $
  if length vals == 1 then show (head vals)
  else
    assert (length names == length vals) $
    intercalate ", " $
    map (\ (n, v) -> n ++ "=" ++ show v) (zip names vals)
  where names = map (symName . rangedData . W.recSym) recs

wrapTag :: String -> String -> String
wrapTag tag str = "<" ++ tag ++ ">" ++ str ++ "</" ++ tag ++ ">"

-- Report missed items for a cross record
reportMisses :: Handle -> Count -> [W.Record] -> [[Integer]] -> IO ()
reportMisses h count recs missing =
  assert (not $ null $ missing) $
  put ("<p>" ++ tag ++ show (length missing) ++
        " misses:</p>" ++
        (if ul then "<ul class='misses'>"
          else "<p class='misses'>")) >>
  rptMiss True (head missing) >>
  mapM_ (rptMiss False) (tail missing) >>
  put (if ul then "</ul>" else "</p>")
  where put = hPutStr h
        partial = countMissed count > toInteger (length missing)
        tag = if partial then "First " else ""
        ul = length recs > 1
        rptMiss first e =
          put $
          if ul then wrapTag "li" $ showMiss recs e
          else if first then wrapTag "span" $ showMiss recs e
          else ", " ++ (wrapTag "span" $ showMiss recs e)

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
grpHeader name gc = wrapTag "h4" $ name ++ " (" ++ fill (covCount gc) ++ ")"
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
