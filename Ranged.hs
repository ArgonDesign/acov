module Ranged
  ( LCPos(..)
  , LCRange(..)
  , Ranged(..) , rangedRange , rangedData , copyRange , wideRange
  ) where

data LCPos = LCPos Int Int
  deriving Show

data LCRange = LCRange LCPos LCPos
  deriving Show

extendRange :: LCRange -> LCRange -> LCRange
extendRange (LCRange lc0 _) (LCRange _ lc1) = LCRange lc0 lc1

data Ranged a = Ranged LCRange a
  deriving Show

instance Functor Ranged where
  fmap f (Ranged rng a) = Ranged rng (f a)

rangedRange :: Ranged a -> LCRange
rangedRange (Ranged rng _) = rng

rangedData :: Ranged a -> a
rangedData (Ranged _ a) = a

copyRange :: Ranged a -> b -> Ranged b
copyRange ra b = Ranged (rangedRange ra) b

wideRange :: Ranged a -> Ranged b -> c -> Ranged c
wideRange (Ranged rng0 _) (Ranged rng1 _) c = Ranged (extendRange rng0 rng1) c
