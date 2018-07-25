module RangeList
  ( RangeList
  , rlLength
  , rlToList
  , rlEmpty
  , rlAdd
  ) where

import Control.Exception.Base
import qualified Data.Map.Strict as Map

data RangeList = RangeList { rlLength :: Integer
                           , rlRanges :: Map.Map Integer Integer
                           }

rlToList :: RangeList -> [Integer]
rlToList rl = concat $ Map.foldrWithKey f [] (rlRanges rl)
  where f lo hi ranges = [lo..hi] : ranges

rlEmpty :: RangeList
rlEmpty = RangeList 0 Map.empty

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
  (Map.fromDescList ((low, high) : pairs))

flatten :: Map.Map Integer Integer -> RangeList
flatten = flattenFinish . Map.foldlWithKey flattenUpd ([], Nothing, 0)
