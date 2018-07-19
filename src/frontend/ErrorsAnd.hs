module ErrorsAnd
  ( ErrorsAnd(..)
  ) where

import Control.Applicative

import Ranged

data ErrorsAnd a = ErrorsAnd [Ranged String] a

instance Functor ErrorsAnd where
  fmap f (ErrorsAnd errs a) = ErrorsAnd errs (f a)

instance Applicative ErrorsAnd where
  pure a = ErrorsAnd [] a
  ErrorsAnd errf f <*> ErrorsAnd erra a = ErrorsAnd (errf ++ erra) (f a)
