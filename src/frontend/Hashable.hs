module Hashable
  ( Hashable
  , hash
  , hashCombine
  ) where

{-
  A simple version of Data.Hashable

  We can't just depend on "hashable" in Cabal because its numbers jump
  with different versions. Also, I'd rather not specify a particular
  hashable version in acov.cabal because then it becomes much harder
  to integrate with package managers.

  Since we don't need anything clever, let's just hack something up
  locally. This doesn't get the funky generics support that the real
  library has :-(
-}
import Data.Bits

hashCombine :: Int -> Int -> Int
hashCombine h1 h2 = (h1 * 16777619) `xor` h2

class Hashable a where
  hash :: a -> Int

instance Hashable Int where
  hash = id

instance Hashable Integer where
  hash n = if n >= minInt && n <= maxInt then
             hash (fromInteger n :: Int)
           else
             hashCombine (fromInteger lo) (hash hi)
    where hi = quot n (shift 1 29)
          lo = rem n (shift 1 29)
          minInt = fromIntegral (minBound :: Int)
          maxInt = fromIntegral (maxBound :: Int)

instance (Hashable a, Hashable b) => Hashable (a, b) where
  hash (a, b) = hashCombine (hash a) (hash b)

instance Hashable a => Hashable [a] where
  hash as = case foldr f (0, 0) as of
              (len, h) -> hashCombine len h
    where f a (len, h) =
            let h' = hashCombine (hash a) h
                len' = len + 1 in
              seq h' $ seq len' $ (len', h')

instance Hashable a => Hashable (Maybe a) where
  hash Nothing = 0
  hash (Just a) = complement (hash a)

instance Hashable Bool where
  hash False = 0
  hash True = 1

instance Hashable Char where
  hash = hash . fromEnum

instance (Hashable a, Hashable b) => Hashable (Either a b) where
  hash (Left a) = hash a
  hash (Right b) = complement (hash b)
