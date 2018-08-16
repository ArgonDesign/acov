module CountPass
  ( Coverage(..)
  , ModCoverage(..)
  , ScopeCoverage(..)
  , GroupCoverage(..)
  , BitsCov(..)
  , covCount
  , run
  ) where

import Control.Exception.Base (assert)
import Data.Bits
import qualified Data.IntSet as IS
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
cbCount :: Int -> IS.IntSet -> IS.IntSet -> Count
cbCount w ones zeros = mkCount hits total
  where hits = IS.size ones + IS.size zeros
        total = toInteger (2 * w)

-- BitsCov w hits partial misses
--
--   where w is the width of the record, hits is the set of valid bits
--   that hit, partial is true if misses is not the whole lot and
--   misses is the first bit indices that miss.
data BitsCov = BitsCov Int IS.IntSet Bool [Int]

mkBC :: Int -> IS.IntSet -> BitsCov
mkBC w hits = seq misses $ BitsCov w hits partial misses
  where partial = assert (IS.size hits <= w) $
                  (w - IS.size hits) > 10
        misses = take 10 $ filter miss [0..(w - 1)]
        miss i = not $ IS.member i hits

data GroupCoverage = RecCov Count [W.Record] [[Integer]]
                   -- BRecCov zeros ones
                   | BRecCov Count BitsCov BitsCov
                   | BadScope

covCount :: GroupCoverage -> Maybe Count
covCount (RecCov c r vs) = Just $ c
covCount (BRecCov c bc0 bc1) = Just $ c
covCount BadScope = Nothing

{-
  This runs through the crossed values, counting up how many of them
  we've managed to hit. We also collect the first 10 values that we
  missed.
-}
countGroup' :: M.GroupCoverage -> GroupCoverage

countGroup' (M.Recs (M.RecsCoverage recs vals)) = RecCov cnt recs misses
  where cnt = crossCount vals cross
        cross = mkCross $ map (\ r -> (W.recWidth r, W.recClist r)) recs
        nToTake = fromInteger (min (countMissed cnt) 10)
        misses = crossMisses nToTake vals cross

countGroup' (M.BRec (M.BRecCoverage brec (ones, zeros))) =
  BRecCov cnt bc0 bc1
  where cnt = cbCount w ones zeros
        w = W.brWidth brec
        bc0 = mkBC w ones
        bc1 = mkBC w zeros

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
