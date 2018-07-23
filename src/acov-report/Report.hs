module Report
  ( report
  ) where

import Control.Exception.Base
import Control.Monad
import Data.Bits
import Data.List
import qualified Data.Set as Set
import System.IO

import Ranged
import SymbolTable

import Parser (symName)
import qualified Width as W
import Merge

report :: Handle -> Coverage -> IO ()
report h (Coverage mods) =
  hPutStr h "<html><head><title>Coverage report</title></head><body>" >>
  mapM_ (reportMod h) mods >>
  hPutStr h "</body></html>"

reportMod :: Handle -> ModCoverage -> IO ()
reportMod h (ModCoverage name scopes) =
  hPutStr h ("<h1>Module " ++ name ++ "</h1>") >>
  mapM_ (reportScope h) scopes

reportScope :: Handle -> ScopeCoverage -> IO ()
reportScope h (ScopeCoverage name grps) =
  assert (not $ null grps) $
  hPutStr h ("<h2>" ++ name ++ "</h2>") >>
  mapM_ (reportGrp h) grps

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
  put ("<h3>" ++ name ++ " (" ++ show hits ++ "/" ++ show count ++ ")</h3>") >>
  if hits /= count then
    assert (not $ null $ firstMisses) $
    put ("<p>First " ++ show (length firstMisses) ++
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
