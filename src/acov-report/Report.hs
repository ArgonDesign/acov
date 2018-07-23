module Report
  ( report
  ) where

import Control.Exception.Base
import Control.Monad
import Data.Bits
import Data.List
import qualified Data.Set as Set
import Data.Time.LocalTime
import Data.Time.Format
import System.IO

import Ranged
import SymbolTable

import Parser (symName)
import qualified Width as W
import Merge

report :: Handle -> Coverage -> IO ()
report h cov =
  do { time <- getZonedTime
     ; put ("<html><head><title>Coverage report</title></head><body>\
            \<h1>Coverage report</h1>\
            \<p>Coverage based on " ++
            show (covCount cov) ++
            " tests; report generated at " ++
            formatTime defaultTimeLocale "%Y/%m/%d %H:%M" time ++
            "</p><h1>Modules</h1>")
     ; mapM_ (reportMod h) (covMods cov)
     ; put "</body></html>"
     }
  where put = hPutStr h

reportMod :: Handle -> ModCoverage -> IO ()
reportMod h (ModCoverage name scopes) =
  hPutStr h ("<h2>Module " ++ name ++ "</h2>") >>
  mapM_ (reportScope h multiScope) scopes
  where multiScope =
          assert (not $ null scopes) $
          not $ null $ tail scopes

reportScope :: Handle -> Bool -> ScopeCoverage -> IO ()
reportScope h multiScope (ScopeCoverage name grps) =
  (if multiScope then hPutStr h ("<h3>" ++ name ++ "</h3>") else return ()) >>
  (assert (not $ null grps) $ mapM_ (reportGrp h) grps)

cross' :: [W.Record] -> [([Integer], Integer, Int)]
cross' recs =
  assert (not $ null recs) $
  if length recs == 1 then map (\ n -> ([n], n, w)) clist
  else concatMap f $ cross' (tail recs)
  where
    r0 = head recs
    w = W.recWidth r0
    clist = W.recClist r0
    f (rst, n', w') = map (\ n -> (n : rst, shiftL n w' + n', w + w')) clist

type Entry = ([Integer], Integer)

cross :: [W.Record] -> [Entry]
cross = map (\ (vals, val, _) -> (vals, val)) . cross'

{-
  This runs through the crossed values, counting up how many of them
  we've managed to hit. We also collect the first 10 values that we
  missed.
-}
countHits :: GroupCoverage -> (Int, Int, [Entry])
countHits gc =
  let (hits, cnt, badleft, bads) =
        foldl' f (0, 0, 10, []) (cross (gcRecs gc)) in
    (hits, cnt, reverse bads)
  where f (hits, cnt, badleft, bads) (vals, val) =
          if Set.member val (gcVals gc) then
            (1 + hits, 1 + cnt, badleft, bads)
          else if badleft > 0 then
            (hits, 1 + cnt, badleft - 1, (vals, val) : bads)
          else
            (hits, 1 + cnt, badleft, bads)

showMiss :: SymbolTable () -> Entry -> String
showMiss st (vals, val) =
  assert (not $ null vals) $
  if length vals == 1 then assert (head vals == val) $ show val
  else
    assert (length names == length vals) $
    intercalate ", " $
    map (\ (n, v) -> n ++ "=" ++ show v) (zip names vals)
  where names = map (symName . rangedData . fst) (stAssocs st)

grpName :: GroupCoverage -> String
grpName gc =
  assert (not $ null recs) $
  if length recs == 1 then recName (head recs)
  else intercalate ", " (map recName recs)
  where recs = gcRecs gc
        recName r = symName $ rangedData $
                    stNameAt (rangedData (W.recSym r)) (gcST gc)

reportGrp :: Handle -> GroupCoverage -> IO ()
reportGrp h gc =
  put ("<h4>" ++ name ++ " (" ++ show hits ++ "/" ++ show count ++ ")</h4>") >>
  if hits /= count then
    assert (not $ null $ firstMisses) $
    put ("<p>" ++ tag ++ show (length firstMisses) ++
         " misses:</p><ul class='misses'>") >>
    mapM_ rptMiss firstMisses >>
    put "</ul>"
  else
    return ()
  where put = hPutStr h
        name = grpName gc
        st = gcST gc
        (hits, count, firstMisses) = countHits gc
        rptMiss e = put $ "<li>" ++ showMiss st e ++ "</li>"
        tag = if count - hits > length (firstMisses) then "First " else ""
