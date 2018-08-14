module Count
  ( Count
  , zeroCount
  , mkCount
  , countFull
  , countMissed
  , showCount
  , Counts
  , zeroCounts
  , showCounts
  , incCounts1
  , incCounts
  ) where

import Control.Exception.Base

-- A pair of integers, counting hits and misses
data Count = Count Int Integer

zeroCount :: Count
zeroCount = Count 0 0

mkCount :: Int -> Integer -> Count
mkCount hit count = assert (toInteger hit <= count) $ Count hit count

countFull :: Count -> Bool
countFull (Count hit count) = toInteger hit == count

countMissed :: Count -> Integer
countMissed (Count hit count) = count - toInteger hit

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

incCounts1 :: Maybe Count -> Counts -> Counts
incCounts1 Nothing counts = counts
incCounts1 (Just c) (Counts tcnt gcnt) =
  Counts (incCount (countFull c) tcnt) (addCount c gcnt)

incCounts :: Counts -> Counts -> Counts
incCounts (Counts tdelta gdelta) (Counts tcnt gcnt) =
  Counts (incCount (countFull tdelta) tcnt) (addCount gdelta gcnt)

showCounts :: String -> Counts -> String
showCounts thing (Counts tcnt vcnt) =
  "<p>Filled " ++ showCount tcnt ++ " " ++ thing ++ "; " ++
  showCount vcnt ++ " cross points.</p>"
