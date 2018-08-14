{-# Language CPP #-}

module RangeList
  ( RangeList
  , rlLength
  , rlIntervals
  , rlToList
  , rlEmpty
  , rlMember
  , rlRange
  , rlAdd
  , ivlListToRangeList
  ) where

import Control.Exception.Base
import qualified Data.Map.Strict as Map
import Data.Hashable

import IvlList

data RangeList = RangeList { rlLength :: Integer
                           , rlRanges :: Map.Map Integer Integer
                           }

instance Hashable RangeList where
  hashWithSalt n rl = hashWithSalt n $ Map.toAscList $ rlRanges rl

rlToList :: RangeList -> [Integer]
rlToList rl = concat $ Map.foldrWithKey f [] (rlRanges rl)
  where f lo hi ranges = [lo..hi] : ranges

rlIntervals :: RangeList -> [(Integer, Integer)]
rlIntervals = Map.toAscList . rlRanges

rlEmpty :: RangeList
rlEmpty = RangeList 0 Map.empty

rlMember :: Integer -> RangeList -> Bool
rlMember n rl =
  case Map.lookupLE n (rlRanges rl) of
    Nothing -> False
    Just (_, hi) -> n <= hi

rlRange :: (Integer, Integer) -> RangeList
rlRange rng = rlAdd rng rlEmpty

rlAdd :: (Integer, Integer) -> RangeList -> RangeList
rlAdd (low, high) rl =
  if high < low then rl
  else flatten $ Map.alter (Just . f) low (rlRanges rl)
  where f (Just high') = max high high'
        f Nothing = high

type FlattenAcc = ([(Integer, Integer)], Maybe (Integer, Integer), Integer)

flattenUpd :: FlattenAcc -> Integer -> Integer -> FlattenAcc
flattenUpd (pairs, Nothing, size) low' high' =
    assert (low' <= high') $
    (pairs, Just (low', high'), size)

flattenUpd (pairs, Just (low, high), size) low' high' =
  assert (low < low') $
  assert (low' <= high') $
  if low' <= high + 1 then (pairs, Just (low, max high high'), size)
  else ((low, high) : pairs, Just (low', high'), size + (high - low + 1))

flattenFinish :: FlattenAcc -> RangeList

flattenFinish (pairs, Nothing, size) =
  assert (size == 0) $
  assert (null $ pairs) $
  rlEmpty

flattenFinish (pairs, Just (low, high), size) =
  RangeList
  (size + (high - low + 1))
#if MIN_VERSION_containers(0,5,9)
  (Map.fromDescList ((low, high) : pairs))
#else
  (Map.fromAscList (reverse ((low, high) : pairs)))
#endif

flatten :: Map.Map Integer Integer -> RangeList
flatten = flattenFinish . Map.foldlWithKey flattenUpd ([], Nothing, 0)

-- Alternative constructor, via an IvlList. If you don't need the
-- intermediate data structures, this should be much more efficient
-- than generating an empty RangeList and adding the intervals one at
-- a time.
ivlListToRangeList :: IvlList -> RangeList
ivlListToRangeList ilist =
  RangeList (ivlListLen ilist) (Map.fromAscList (ivlListIntervals ilist))
