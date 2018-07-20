module Report
  ( report
  ) where

import Control.Exception.Base
import System.IO

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

reportGrp :: Handle -> GroupCoverage -> IO ()
reportGrp h gc = hPutStr h "<p>Group</p>"
