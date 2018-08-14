module CountPass
  ( Coverage(..)
  , ModCoverage(..)
  , ScopeCoverage(..)
  , GroupCoverage(..)
  , GroupCount(..)
  , covCount
  , run
  ) where

import Data.Bits
import qualified Data.Set as Set
import qualified Data.Foldable as Foldable

import Count
import Cross
import Ranged

import qualified Merge as M
import qualified Width as W
import Parser (symName)

{-
  When we have a set of integers but are doing a "cover bits" group,
  we need to iterate through, looking for which bits have been seen
  set and cleared.
-}
cbCount :: Int -> Set.Set Int -> Set.Set Int -> Count
cbCount w ones zeros = mkCount hits total
  where hits = Set.size ones + Set.size zeros
        total = toInteger (2 * w)

cbMissing :: Int -> Set.Set Int -> Set.Set Int -> [(Int, Bool)]
cbMissing w ones zeros = missing True ones ++ missing False zeros
  where missing isOne vals =
          map (\ i -> (i, isOne)) $ filter (bad vals) [0..(w - 1)]
        bad vals i = not $ Set.member i vals

data GroupCount a = GroupCount Count a

gcCount :: GroupCount a -> Count
gcCount (GroupCount count _) = count

data GroupCoverage = RecCov (GroupCount ([W.Record], [[Integer]]))
                   | BRecCov (GroupCount (W.BitsRecord, [(Int, Bool)]))
                   | BadScope

covCount :: GroupCoverage -> Maybe Count
covCount (RecCov gc) = Just $ gcCount gc
covCount (BRecCov gc) = Just $ gcCount gc
covCount BadScope = Nothing

{-
  This runs through the crossed values, counting up how many of them
  we've managed to hit. We also collect the first 10 values that we
  missed.
-}
countGroup' :: M.GroupCoverage -> GroupCoverage

countGroup' (M.Recs (M.RecsCoverage recs vals)) =
  RecCov (GroupCount cnt (recs, misses))
  where cnt = crossCount vals cross
        cross = mkCross $ map (\ r -> (W.recWidth r, W.recClist r)) recs
        nToTake = fromInteger (min (countMissed cnt) 10)
        misses = crossMisses nToTake vals cross

countGroup' (M.BRec (M.BRecCoverage brec (ones, zeros))) =
  BRecCov (GroupCount cnt (brec, bads))
  where w = W.brWidth brec
        cnt = cbCount w ones zeros
        bads = take 10 $ cbMissing w ones zeros

countGroup' M.BadScope = BadScope

countGroup :: (String, M.GroupCoverage) -> (String, GroupCoverage)
countGroup (grpName, gc) = (grpName, countGroup' gc)

data ScopeCoverage = ScopeCoverage { scName :: String
                                   , scCounts :: Counts
                                   , scGroups :: [(String, GroupCoverage)]
                                   }

countScope :: M.ScopeCoverage -> ScopeCoverage
countScope (M.ScopeCoverage name gcs) = ScopeCoverage name scnts gcs'
  where gcs' = map countGroup gcs
        scnts = foldr incCounts1 zeroCounts (map (covCount . snd) gcs')

data ModCoverage = ModCoverage { mcName :: String
                               , mcCounts :: Counts
                               , mcScopes :: [ScopeCoverage]
                               }

countMod :: M.ModCoverage -> ModCoverage
countMod (M.ModCoverage name scs) = ModCoverage name mcnts scs'
  where scs' = map countScope scs
        mcnts = foldr incCounts zeroCounts (map scCounts scs')

data Coverage = Coverage { covTests :: Int
                         , covCounts :: Counts
                         , covMods :: [ModCoverage]
                         }

run :: M.Coverage -> Coverage
run cov = Coverage (M.covTests cov) cnts mcs
  where mcs = map countMod (M.covMods cov)
        cnts = foldr incCounts zeroCounts (map mcCounts mcs)
