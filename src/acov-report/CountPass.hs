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
cbHits :: Set.Set Integer -> (Integer, Integer)
cbHits = Foldable.foldr f (0, 0)
  where f val (ones, zeros) = (ones .|. val, zeros .|. (complement val))

cbCount :: Int -> (Integer, Integer) -> Count
cbCount w (ones, zeros) = mkCount hits total
  where mask = shift (1 :: Integer) w - 1
        hits = popCount (ones .&. mask) + popCount (zeros .&. mask)
        total = toInteger (2 * w)

zBits :: Bool -> Int -> Integer -> [(Int, Bool)]
zBits b w n = take 10 $ f (w - 1)
  where f i =
          if i < 0 then
            []
          else if testBit n i then
            f (i - 1)
          else
            (i, b) : f (i - 1)

cbMissing :: Int -> (Integer, Integer) -> [(Int, Bool)]
cbMissing w (ones, zeros) = zBits True w ones ++ zBits False w zeros

data GroupCoverage =
  GroupCoverage { gcVals :: Set.Set Integer
                , gcCount :: Count
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

countGroup (M.GroupCoverage gvals (Left recs)) =
  GroupCoverage gvals cnt (Left (recs, misses))
  where cnt = crossCount gvals cross
        cross = mkCross $ map (\ r -> (W.recWidth r, W.recClist r)) recs
        nToTake = fromInteger (min (countMissed cnt) 10)
        misses = crossMisses nToTake gvals cross

countGroup (M.GroupCoverage gvals (Right brec)) =
  GroupCoverage gvals cnt $ Right (brec, bads)
  where oz = cbHits gvals
        w = W.brWidth brec
        cnt = cbCount w oz
        bads = take 10 $ cbMissing w oz

gcName :: GroupCoverage -> String
gcName (GroupCoverage _ _ (Left (recs, _))) =
  if length recs == 1 then recName (head recs)
  else intercalate ", " (map recName recs)
  where recName = symName . rangedData . W.recSym
gcName (GroupCoverage _ _ (Right (brec, _))) =
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
