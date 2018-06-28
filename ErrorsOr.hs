module ErrorsOr
  ( ErrorsOr
  , bad
  , good
  , mapEO
  ) where

import Ranged

newtype ErrorsOr a = ErrorsOr (Either [Ranged String] a)
  deriving Show

bad :: Ranged String -> ErrorsOr a
bad rstr = ErrorsOr $ Left [rstr]

good :: a -> ErrorsOr a
good a = ErrorsOr $ Right a

mapEO :: (a -> ErrorsOr b) -> [a] -> ErrorsOr [b]
mapEO f as =
  case foldr take (Right []) as of
    Left errs -> ErrorsOr $ Left errs
    Right bs -> good bs
  where take a ebs =
          case f a of
            ErrorsOr (Left errs') ->
              case ebs of Left errs -> Left (errs' ++ errs)
                          Right vals -> Left errs'
            ErrorsOr (Right b) ->
              case ebs of Left errs -> Left errs
                          Right bs -> Right (b : bs)

instance Functor ErrorsOr where
  fmap f (ErrorsOr (Left errs)) = ErrorsOr $ Left errs
  fmap f (ErrorsOr (Right a)) = ErrorsOr $ Right $ f a
