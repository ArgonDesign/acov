module Report
  ( report
  ) where

import Control.Exception.Base
import Data.List
import System.IO

import Count
import Ranged
import Time (stringTime)

import Parser (symName)
import qualified Width as W
import CountPass

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
     }
  where put = hPutStr h
        scopes = mcScopes mc

reportScope :: Handle -> Bool -> ScopeCoverage -> IO ()
reportScope h singleScope sc =
  do { if not singleScope then put ("<h3>" ++ (scName sc) ++ "</h3>")
       else return ()
     ; put (showCounts "groups" (scCounts sc))
     ; mapM_ (reportGrp h) (scGroups sc)
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

showMissBit :: (Int, Bool) -> String
showMissBit (i, b) = "bit " ++ show i ++ (if b then " set" else " clear")

wrapTag :: String -> String -> String
wrapTag tag str = "<" ++ tag ++ ">" ++ str ++ "</" ++ tag ++ ">"

reportMisses :: Handle -> GroupCoverage -> IO ()
reportMisses h (GroupCoverage count (Left (recs, missing))) =
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

reportMisses h (GroupCoverage count (Right (brec, bads))) =
  assert (not $ null $ bads) $
  put ("<p>" ++ tag ++ show (length bads) ++
        " misses:</p>" ++
        "<p class='misses'>") >>
  rptMiss True (head bads) >>
  mapM_ (rptMiss False) (tail bads) >>
  put "</p>"
  where put = hPutStr h
        partial = countMissed count > toInteger (length bads)
        tag = if partial then "First " else ""
        rptMiss first e =
          put $
          (if first then "" else ", ") ++
          (wrapTag "span" $ showMissBit e)

reportGrp :: Handle -> GroupCoverage -> IO ()
reportGrp h gc =
  put (wrapTag "h4" $ name ++ " (" ++ showCount count ++ ")") >>
  if countFull count then
    return ()
  else
    reportMisses h gc
  where put = hPutStr h
        name = gcName gc
        count = gcCount gc
