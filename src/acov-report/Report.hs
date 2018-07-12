module Report
  ( CoverReport
  , mkCoverReport
  , dumpCoverReport
  ) where

import Control.Exception.Base
import Data.Bits
import Data.Foldable
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.IO

import Coverage
import Parser
import Ranged

data ScopedCoverReport =
  ScopedCoverReport { scrHit :: Set.Set Integer
                    , scrMiss :: Set.Set Integer
                    }

newtype CoverReport = CoverReport (Map.Map String ScopedCoverReport)

coverTargets :: Int -> Maybe CoverList -> Set.Set Integer
coverTargets width Nothing =
  assert (width <= 10) Set.fromList [0..(shift 1 width) - 1]
coverTargets _ (Just (CoverList vars)) =
  Set.fromList $ map (toInteger . rangedData) vars

isInt :: Integer -> Bool
isInt n = toInteger (minBound :: Int) <= n && n < toInteger (maxBound :: Int)

mkSCR :: Set.Set Integer -> Set.Set Integer -> ScopedCoverReport
mkSCR tgts hits =
  ScopedCoverReport (Set.intersection tgts hits) (Set.difference tgts hits)

mkCoverReport :: PerModCoverage ->
                 String -> String -> Int -> Maybe CoverList -> CoverReport
mkCoverReport pmc modName varName width clist =
  CoverReport $ Map.map (mkSCR targets) hits
  where targets = coverTargets width clist
        hits = pmcLookup pmc modName varName

dumpIntSet :: Handle -> Set.Set Integer -> IO ()
dumpIntSet handle ints = foldlM dumpInt True ints >> return ()
  where dumpInt True n = hPutStr handle (show n) >> return False
        dumpInt False n = hPutStr handle (", " ++ show n) >> return False

dumpSCRRow :: Handle -> String -> Set.Set Integer -> IO ()
dumpSCRRow handle name ints =
  if Set.null ints then return ()
  else put ("<tr><td>" ++ name ++ ":</td><td>") >>
       dumpIntSet handle ints >>
       put "</td></tr>"
  where put = hPutStr handle

dumpSCR :: Handle -> Bool -> String ->
           (String, ScopedCoverReport) -> IO ()
dumpSCR handle showScope modvar (scope, scr) =
  if showScope then put hdr else return () >>
  put ("<table class='cover-report' id='cover-report-" ++ modvar ++ "'>") >>
  dumpSCRRow handle "Hit" (scrHit scr) >>
  dumpSCRRow handle "Missed" (scrMiss scr) >>
  put "</table>"
  where put = hPutStr handle
        hdr = "<h3 class='cover-report'>Scope " ++ scope ++ "</h3>"

dumpCoverReport :: Handle -> String -> String -> CoverReport -> IO ()
dumpCoverReport handle modName varName (CoverReport cr) =
  put ("<h2 class='cover-report'>" ++ modName ++ "." ++ varName ++ "</h2>") >>
  mapM_ (dumpSCR handle showScope (modName ++ "-" ++ varName))
  (Map.toList cr)
  where put = hPutStr handle
        showScope = Map.size cr > 1
