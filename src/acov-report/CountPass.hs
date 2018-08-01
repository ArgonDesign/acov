module CountPass
  ( Coverage(..)
  , ModCoverage(..)
  , ScopeCoverage(..)
  , GroupCoverage(..)
  , gcName
  , run
  ) where

import Data.Bits
import Data.List
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

data GroupCoverage =
  GroupCoverage { gcCount :: Count
                , gcBody :: Either
                            ([W.Record], [[Integer]])
                            (W.BitsRecord, [(Int, Bool)])
                }

{-
  This runs through the crossed values, counting up how many of them
  we've managed to hit. We also collect the first 10 values that we
  missed.
-}
countGroup :: M.GroupCoverage -> GroupCoverage

countGroup (M.GroupCoverage (Left (M.RecsCoverage recs vals))) =
  GroupCoverage cnt $ Left (recs, misses)
  where cnt = crossCount vals cross
        cross = mkCross $ map (\ r -> (W.recWidth r, W.recClist r)) recs
        nToTake = fromInteger (min (countMissed cnt) 10)
        misses = crossMisses nToTake vals cross

countGroup (M.GroupCoverage (Right (M.BRecCoverage brec (ones, zeros)))) =
  GroupCoverage cnt $ Right (brec, bads)
  where w = W.brWidth brec
        cnt = cbCount w ones zeros
        bads = take 10 $ cbMissing w ones zeros

gcName :: GroupCoverage -> String
gcName (GroupCoverage _ (Left (recs, _))) =
  if length recs == 1 then recName (head recs)
  else intercalate ", " (map recName recs)
  where recName = symName . rangedData . W.recSym
gcName (GroupCoverage _ (Right (brec, _))) =
  (symName $ rangedData $ W.brSym brec) ++ " bits"

data ScopeCoverage = ScopeCoverage { scName :: String
                                   , scCounts :: Counts
                                   , scGroups :: [GroupCoverage]
                                   }

countScope :: M.ScopeCoverage -> ScopeCoverage
countScope (M.ScopeCoverage name gcs) = ScopeCoverage name scnts gcs'
  where gcs' = map countGroup gcs
        scnts = foldr incCounts1 zeroCounts (map gcCount gcs')

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
