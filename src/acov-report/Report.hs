module Report
  ( report
  ) where

import Merge
import System.IO

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
  hPutStr h ("<h2>" ++ name ++ "</h2>") >>
  mapM_ (reportGrp h) grps

reportGrp :: Handle -> GroupCoverage -> IO ()
reportGrp h gc = hPutStr h "<p>Group</p>"
