module Ranged
  ( LCPos(..)
  , LCRange(..) , extendRange
  , Ranged(..) , rangedRange , rangedData , copyRange , wideRange
  ) where

import Hashable

data LCPos = LCPos Int Int

instance Hashable LCPos where
  hash (LCPos l c) = hashCombine (hash l) (hash c)

data LCRange = LCRange LCPos LCPos

instance Hashable LCRange where
  hash (LCRange a b) = hashCombine (hash a) (hash b)

extendRange :: LCRange -> LCRange -> LCRange
extendRange (LCRange lc0 _) (LCRange _ lc1) = LCRange lc0 lc1

data Ranged a = Ranged LCRange a

instance Hashable a => Hashable (Ranged a) where
  hash (Ranged rng a) = hashCombine (hash rng) (hash a)

rangedRange :: Ranged a -> LCRange
rangedRange (Ranged rng _) = rng

rangedData :: Ranged a -> a
rangedData (Ranged _ a) = a

copyRange :: Ranged a -> b -> Ranged b
copyRange ra b = Ranged (rangedRange ra) b

wideRange :: Ranged a -> Ranged b -> c -> Ranged c
wideRange (Ranged rng0 _) (Ranged rng1 _) c = Ranged (extendRange rng0 rng1) c
