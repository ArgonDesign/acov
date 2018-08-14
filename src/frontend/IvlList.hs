module IvlList
  ( IvlList
  , makeIvlList
  , ivlListIntervals
  , ivlListLen
  ) where

import Data.List (sort)

-- An interval list is a handy intermediate data structure between an
-- actual list of intervals (stored as pairs (LO, HI)) and a
-- RangeList.
--
-- If there are N intervals in the input, it can be constructed in
-- O(N*log N) time and then converted to a RangeList proper in the
-- same O(N*log N) time.

-- An IvlList is stored as an ordered list of disjoint intervals
-- (ascending).
data IvlList = IvlList [(Integer, Integer)]

nonemptyIvls :: [(Integer, Integer)] -> [(Integer, Integer)]
nonemptyIvls = filter nonempty
  where nonempty (a, b) = a <= b

-- mergeIvls runs in time O(N). Given a sorted list of nonempty
-- intervals represented as pairs (LO, HI), it returns a sorted,
-- disjoint list of intervals with the same union.
--
-- Note that the sorted nonempty criterion means that if there are
-- intervals I = (a, b) and J = (c, d) where I comes before J in the
-- list then a <= c. However, we don't know that b <= d (for example
-- I=(0, 10) and J=(2, 3)), which is why we need the call to max.
mergeIvls :: [(Integer, Integer)] -> [(Integer, Integer)]
mergeIvls [] = []
mergeIvls (ivl : ivls) = mi ivl ivls
  where mi (a, b) [] = [(a, b)]
        mi (a, b) ((c, d) : rst) =
          if b + 1 < c then
            (a, b) : mi (c, d) rst
          else
            mi (a, max b d) rst

makeIvlList :: [(Integer, Integer)] -> IvlList
makeIvlList = IvlList . mergeIvls . sort . nonemptyIvls

ivlListIntervals :: IvlList -> [(Integer, Integer)]
ivlListIntervals (IvlList ivls) = ivls

ivlListLen :: IvlList -> Integer
ivlListLen (IvlList ivls) = foldr (+) 0 $ map ivlLen ivls
  where ivlLen (a, b) = b - a + 1
