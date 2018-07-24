module Report
  ( report
  ) where

import Control.Exception.Base
import Data.List
import System.IO

import Ranged
import SymbolTable
import Time (stringTime)

import Parser (symName)
import qualified Width as W
import Count

report :: Handle -> Coverage -> IO ()
report h cov =
  do { time <- stringTime
     ; put ("<html><head><title>Coverage report</title></head><body>\
            \<h1>Coverage report</h1>\
            \<p>Coverage based on " ++
            show (covTests cov) ++
            " tests; report generated at " ++ time ++ "</p><p>")
     ; put (showCounts "modules" (covCounts cov))
     ; put ("</p><h1>Modules</h1>")
     ; mapM_ (reportMod h) (covMods cov)
     ; put "</body></html>"
     }
  where put = hPutStr h
        mods = covMods cov

reportMod :: Handle -> ModCoverage -> IO ()
reportMod h mc =
  do { put $ "<h2>Module " ++ (mcName mc) ++ "</h2>"
     ; if multiScope then put (showCounts "scopes" (mcCounts mc))
       else return ()
     ; mapM_ (reportScope h multiScope) (mcScopes mc)
     }
  where put = hPutStr h
        multiScope = not $ null $ tail (mcScopes mc)

reportScope :: Handle -> Bool -> ScopeCoverage -> IO ()
reportScope h multiScope sc =
  do { if multiScope then put ("<h3>" ++ (scName sc) ++ "</h3>")
       else return ()
     ; put (showCounts "groups" (scCounts sc))
     ; mapM_ (reportGrp h) (scGroups sc)
     }
  where put = hPutStr h

showMiss :: SymbolTable () -> [Integer] -> String
showMiss st vals =
  assert (not $ null vals) $
  if length vals == 1 then show (head vals)
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

wrapTag :: String -> String -> String
wrapTag tag str = "<" ++ tag ++ ">" ++ str ++ "</" ++ tag ++ ">"

reportGrp :: Handle -> GroupCoverage -> IO ()
reportGrp h gc =
  put (wrapTag "h4" $ name ++ " (" ++ showCount count ++ ")") >>
  if countFull count then
    return ()
  else
    assert (not $ null $ firstMisses) $
    put ("<p>" ++ tag ++ show (length firstMisses) ++
         " misses:</p>" ++
         (if useUL then "<ul class='misses'>"
          else "<p class='misses'>")) >>
    rptMiss True (head firstMisses) >>
    mapM_ (rptMiss False) (tail firstMisses) >>
    put (if useUL then "</ul>" else "</p>")
  where put = hPutStr h
        firstMisses = gcMisses gc
        name = grpName gc
        count = gcCount gc
        st = gcST gc
        useUL = length (gcRecs gc) > 1
        rptMiss first e =
          put $
          if useUL then wrapTag "li" $ showMiss st e
          else if first then wrapTag "span" $ showMiss st e
          else ", " ++ (wrapTag "span" $ showMiss st e)
        tag = if countMissed count > length (firstMisses) then "First " else ""
