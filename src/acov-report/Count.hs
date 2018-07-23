module Count
  ( Coverage(..)
  , ModCoverage(..)
  , ScopeCoverage(..)
  , GroupCoverage(..)
  , run
  , Count
  , countFull
  , countMissed
  , showCount
  , Counts
  , showCounts
  ) where

import Control.Exception.Base
import Data.Bits
import Data.List
import qualified Data.Set as Set

import SymbolTable

import qualified Merge as M
import qualified Width as W

-- A pair of integers, counting hits and misses
data Count = Count Int Int

zeroCount :: Count
zeroCount = Count 0 0

countFull :: Count -> Bool
countFull (Count hit count) = hit == count

countMissed :: Count -> Int
countMissed (Count hit count) = count - hit

incCount :: Bool -> Count -> Count
incCount True (Count hit count) = Count (1 + hit) (1 + count)
incCount False (Count hit count) = Count hit (1 + count)

addCount :: Count -> Count -> Count
addCount (Count hit0 count0) (Count hit1 count1) =
  Count (hit0 + hit1) (count0 + count1)

showCount :: Count -> String
showCount (Count hit count) = show hit ++ " / " ++ show count

-- A pair of counters. The first counts the immediate descendents
-- ("how many scopes were hit?" or similar). The second counts the
-- bottom level (coverage points).
data Counts = Counts Count Count

zeroCounts :: Counts
zeroCounts = Counts zeroCount zeroCount

incCounts1 :: Count -> Counts -> Counts
incCounts1 c (Counts tcnt gcnt) =
  Counts (incCount (countFull c) tcnt) (addCount c gcnt)

incCounts :: Counts -> Counts -> Counts
incCounts (Counts tdelta gdelta) (Counts tcnt gcnt) =
  Counts (incCount (countFull tdelta) tcnt) (addCount gdelta gcnt)

showCounts :: String -> Counts -> String
showCounts thing (Counts tcnt vcnt) =
  "<p>Filled " ++ showCount tcnt ++ " " ++ thing ++ "; " ++
  showCount vcnt ++ " cross points.</p>"

data GroupCoverage = GroupCoverage { gcVals :: Set.Set Integer
                                   , gcST :: SymbolTable ()
                                   , gcRecs :: [W.Record]
                                   , gcCount :: Count
                                   , gcMisses :: [[Integer]]
                                   }

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

type CoverPoint = ([Integer], Integer)

cross :: [W.Record] -> [CoverPoint]
cross = map (\ (vals, val, _) -> (vals, val)) . cross'

{-
  This runs through the crossed values, counting up how many of them
  we've managed to hit. We also collect the first 10 values that we
  missed.
-}
countGroup :: M.GroupCoverage -> GroupCoverage
countGroup gc = GroupCoverage (M.gcVals gc) (M.gcST gc) (M.gcRecs gc)
                cnt (reverse (map fst bads))
  where f (cnt, badleft, bads) (vals, val) =
          if Set.member val (M.gcVals gc) then
            (incCount True cnt, badleft, bads)
          else if badleft > 0 then
            (incCount False cnt, badleft - 1, (vals, val) : bads)
          else
            (incCount False cnt, badleft, bads)
        (cnt, badleft, bads) =
          foldl' f (zeroCount, 10, []) (cross (M.gcRecs gc))

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
