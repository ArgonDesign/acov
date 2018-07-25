module TestRangeList
  ( tests ) where

import Control.Exception.Base
import Data.Bits
import qualified Data.Set as Set

import RangeList

import Test.QuickCheck
import Test.Framework.Providers.API
import Test.Framework.Providers.QuickCheck2

{-
  Some quickcheck based test code for the RangeList module.

  First, let's make a generator for pairs (that will be used as ranges
  to add into the range list). Since we want to test against a simple
  Set-based implementation (where you don't want millions of
  elements), we constrain the length to 2^size.
-}
genPair :: Gen (Integer, Integer)
genPair =
  sized (\ n ->
            do { lo <- oneof
                       [ arbitrary
                       , choose (-100, 100)
                       ]
               ; hi <- frequency
                       [ (1, min (lo - 1) <$> arbitrary)
                       , (10, choose (lo, lo + shiftL lo n))
                       ]
               ; return (lo, hi)
               })

newtype RLTest = RLTest [(Integer, Integer)]
  deriving Show

instance Arbitrary RLTest where
  arbitrary = RLTest <$> listOf (scale (\ n -> quot n 10) genPair)

newtype EasyRL = EasyRL (Set.Set Integer)

erlLength :: EasyRL -> Integer
erlLength (EasyRL set) = toInteger $ Set.size set

erlToList :: EasyRL -> [Integer]
erlToList (EasyRL set) = Set.toAscList set

erlEmpty :: EasyRL
erlEmpty = EasyRL Set.empty

erlAdd :: (Integer, Integer) -> EasyRL -> EasyRL
erlAdd (lo, hi) (EasyRL set) =
  assert (hi - lo < 65536) $
  EasyRL $ foldr Set.insert set [lo..hi]

prop_RLMatchesERL :: RLTest -> Bool
prop_RLMatchesERL (RLTest pairs) =
  (erlLength erl == rlLength rl) && (erlToList erl == rlToList rl)
  where erl = foldr erlAdd erlEmpty pairs
        rl = foldr rlAdd rlEmpty pairs

tests :: [ Test ]
tests = [ testProperty "RangeList test" prop_RLMatchesERL ]
