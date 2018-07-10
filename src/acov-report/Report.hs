module Report
  ( CoverReport
  , mkCoverReport
  , dumpCoverReport
  ) where

import Control.Exception.Base
import Data.Bits
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.IntSet as IS

import Coverage
import Parser
import Ranged

type CoverMap a = Map.Map a Bool
newtype ScopedCoverReport = ScopedCoverReport (CoverMap Integer)

newtype CoverReport = CoverReport (Map.Map String ScopedCoverReport)

coverTargets :: Int -> Maybe CoverList -> [Integer]
coverTargets width Nothing = assert (width <= 10) [0..(shift 1 width) - 1]
coverTargets _ (Just (CoverList vars)) = sort $ map (toInteger . rangedData) vars

isInt :: Integer -> Bool
isInt n = toInteger (minBound :: Int) <= n && n < toInteger (maxBound :: Int)

mkCoverMap :: [Integer] -> IS.IntSet -> CoverMap Integer
mkCoverMap tgts hits = Map.fromAscList $ map chk tgts
  where chk tgt = (tgt, isInt tgt && IS.member (fromInteger tgt) hits)

mkCoverReport :: PerModCoverage ->
                 String -> String -> Int -> Maybe CoverList -> CoverReport
mkCoverReport pmc modName varName width clist =
  CoverReport (Map.map mkSCR hits)
  where targets = coverTargets width clist
        hits = pmcLookup pmc modName varName
        mkSCR vals = ScopedCoverReport (mkCoverMap targets vals)


dumpCoverReport :: String -> String -> CoverReport -> IO ()
dumpCoverReport modName varName (CoverReport cr) =
  putStr $ modName ++ "." ++ varName ++ "\n"
