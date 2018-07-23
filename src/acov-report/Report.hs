module Report
  ( report
  ) where

import Control.Exception.Base
import Data.Bits
import qualified Data.Set as Set
import System.IO

import qualified Width as W
import Merge

report :: Handle -> Coverage -> IO ()
report h (Coverage mods) =
  hPutStr h "<html><head><title>Coverage report</title></head><body>" >>
  mapM_ (reportMod h) mods >>
  hPutStr h "</body></html>"

reportMod :: Handle -> ModCoverage -> IO ()
reportMod h (ModCoverage name scopes) =
  hPutStr h ("<h1>" ++ name ++ "</h1>") >>
  mapM_ (reportScope h) scopes

reportScope :: Handle -> ScopeCoverage -> IO ()
reportScope h (ScopeCoverage name grps) =
  assert (not $ null grps) $
  hPutStr h ("<h2>" ++ name ++ "</h2>") >>
  reportGrp h (head grps) >>
  mapM_ (\ g -> hPutStr h "<hr/>" >> reportGrp h g) grps

cross :: [W.Record] -> [([Integer], Integer, Int)]
cross recs =
  assert (not $ null recs) $
  if length recs == 1 then map (\ n -> ([n], n, w)) clist
  else concatMap f $ cross (tail recs)
  where
    r0 = head recs
    w = W.recWidth r0
    clist = W.recClist r0
    f (rst, n', w') = map (\ n -> (n : rst, shiftL n w' + n', w + w')) clist

showLine :: GroupCoverage -> ([Integer], Integer, Int) ->
             (Int, Int, [String]) -> (Int, Int, [String])
showLine gc (vars, val, w) (hits, count, strs) =
  (if isHit then 1 + hits else hits, 1 + count,
   ("<span class='cov-" ++ hitmiss ++ "'>" ++ tickcross ++ "</span>") : strs)
  where isHit = Set.member val (gcVals gc)
        hitmiss = if isHit then "1" else "0"
        tickcross = if isHit then tick else cross
        tick = "&#x2713;"
        cross = "&#x274c;"

reportGrp :: Handle -> GroupCoverage -> IO ()
reportGrp h gc =
  put ("<p>Group (" ++ show hits ++ "/" ++ show count ++ ")</p><table>") >>
  mapM_ reportLine items >>
  put "</table>"
  where put = hPutStr h
        (hits, count, items) = foldr (showLine gc) (0, 0, []) (cross (gcRecs gc))
        reportLine str = put ("<tr><td>" ++ str ++ "</td></tr>")
