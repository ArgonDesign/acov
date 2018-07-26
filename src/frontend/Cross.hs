module Cross
  ( Cross
  , mkCross
  , crossSize
  , crossCount
  , crossMisses
  ) where

import Control.Exception.Base
import Data.Bits
import qualified Data.Foldable as Foldable
import qualified Data.Set as Set

import RangeList
import Count

newtype Cross = Cross [(Int, RangeList)]

mkCross :: [(Int, RangeList)] -> Cross
mkCross wrls = assert (not $ null wrls) $ Cross wrls

crossSize :: Cross -> Integer
crossSize (Cross rls) = product (map (rlLength . snd) rls)

{-
  This function tests whether a given integer hits a value that was
  expected by the cross. The first argument is a list of widths (MSB
  first) that describe how a number in this cross gets packed into an
  integer.
-}

hitsCross :: Cross -> Integer -> Bool
hitsCross (Cross wrls) n =
  let (n', good) = foldr f (n, True) wrls in
    assert (n' == 0) $ good
  where f (w, rl) (n', good) =
          (shiftR n' w, good && rlMember (mask w .&. n') rl)
        mask w = shiftL (1 :: Integer) w - 1

{-
  Using hitsCross, we can now count the number of elements that are
  hit from a cross, given a set of values.

  The trick is that, instead of iterating over the possibly enormous
  cross, we iterate over the things we've seen. Since they're stored
  in a set, there are no duplicates, so we can count the hits and
  calculate the total number of points.
-}
crossCount :: Set.Set Integer -> Cross -> Count
crossCount vals cross = mkCount (Foldable.foldr f 0 vals) (crossSize cross)
  where f val k = if hitsCross cross val then 1 + k else k

{-
  If we have some missing points, we need to list the first few. To do
  so, we use crossList. This should be lazy, so "take 10 crossList" or
  similar should run quickly even if the full list is enormous.
-}
crossList :: Cross -> [[Integer]]
crossList (Cross wrls) =
  assert (not $ null wrls) $
  let lst = rlToList $ snd $ head wrls in
    if null $ tail wrls then map (\ n -> ([n])) lst
    else concatMap (\ vals -> map (\ n -> n : vals) lst) $
         crossList (Cross (tail wrls))

crossWidths :: Cross -> [Int]
crossWidths (Cross wrls) = map fst wrls

{-
  With crossList in hand, we can find missing points. Unlike
  crossCount, we iterate over the (head of) the whole cross list,
  checking against the set of hits rather than the other way around.
-}
glue :: [Int] -> [Integer] -> Integer
glue widths us = g (zip widths us) 0
  where g [] x = x
        g ((w, u):wus) x = shiftL x w .|. u

crossMisses :: Int -> Set.Set Integer -> Cross -> [[Integer]]
crossMisses n hits cross = f n (crossList cross)
  where f k [] = []
        f 0 _ = []
        f k (v:vs) = if Set.member (glue widths v) hits
                     then f (k - 1) vs
                     else v : (f (k - 1) vs)
        widths = crossWidths cross
