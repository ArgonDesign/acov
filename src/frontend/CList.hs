module CList
  ( CList
  , clCons
  , clLen
  , clEmpty
  , clArray
  , clMap
  , clAt
  ) where

import Data.Array

data CList a = CList Int [a]

clCons :: a -> CList a -> CList a
clCons a (CList n as) = CList (n + 1) (a : as)

clLen :: CList a -> Int
clLen (CList n _) = n

clEmpty :: CList a
clEmpty = CList 0 []

clArray :: CList a -> Array Int a
clArray (CList n as) = listArray (0, n - 1) (reverse as)

clMap :: (a -> b) -> CList a -> CList b
clMap f (CList n as) = CList n $ map f as

clAt :: Int -> CList a -> a
clAt idx (CList n as) = as !! (n - 1 - idx)
