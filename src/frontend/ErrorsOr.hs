module ErrorsOr
  ( ErrorsOr
  , bad , bad1 , good
  , eaToEO , eoToEA
  , foldEO , mapEO , amapEO , traverseEO
  , eoMaybe
  , reportEO
  ) where

import Control.Applicative
import Control.Exception.Base
import Data.Array
import qualified Data.Foldable as F
import Data.Traversable
import System.Exit
import System.IO

import ErrorsAnd
import Ranged

newtype ErrorsOr a = ErrorsOr (Either [Ranged String] a)

bad :: [Ranged String] -> ErrorsOr a
bad errs = assert (not $ null errs)
           ErrorsOr $ Left errs

bad1 :: Ranged String -> ErrorsOr a
bad1 rs = ErrorsOr $ Left [rs]

good :: a -> ErrorsOr a
good = ErrorsOr . Right

instance Functor ErrorsOr where
  fmap f (ErrorsOr (Left errs)) = bad errs
  fmap f (ErrorsOr (Right a)) = good (f a)

instance Applicative ErrorsOr where
  pure a = good a
  ErrorsOr (Left e0) <*> ErrorsOr (Left e1) = bad $ e0 ++ e1
  ErrorsOr (Left e0) <*> ErrorsOr (Right a) = bad $ e0
  ErrorsOr (Right f) <*> ErrorsOr (Left e1) = bad $ e1
  ErrorsOr (Right f) <*> ErrorsOr (Right a) = good $ f a

instance Monad ErrorsOr where
  ErrorsOr (Left errs) >>= f = bad errs
  ErrorsOr (Right a) >>= f = f a
  return = good

updateEO :: (b -> a -> ErrorsOr b) ->
            ([Ranged String], b) -> a -> ([Ranged String], b)
updateEO f (errs, b) a =
  case f b a of
    ErrorsOr (Left errs') -> (errs' ++ errs, b)
    ErrorsOr (Right b') -> (errs, b')

eaToEO :: ErrorsAnd a -> ErrorsOr a
eaToEO (ErrorsAnd errs a) = if null errs then good a else bad errs

eoToEA :: a -> ErrorsOr a -> ErrorsAnd a
eoToEA a0 (ErrorsOr (Left errs)) = ErrorsAnd errs a0
eoToEA _ (ErrorsOr (Right a)) = ErrorsAnd [] a

foldEO :: F.Foldable t => (b -> a -> ErrorsOr b) -> b -> t a -> ErrorsOr b
foldEO f b0 as =
  if null errs then good b1 else bad (reverse errs)
  where (errs, b1) = F.foldl' (updateEO f) ([], b0) as

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

amapEO :: Ix i =>
          (e' -> ErrorsOr e) -> Array i e' -> ErrorsOr (Array i e)
amapEO f arr =
  foldEO map1 [] arr >>= (return . listArray (bounds arr) . reverse)
  where map1 bs a = do { b <- f a; return $ b : bs }

traverseEO :: Traversable t => (a -> ErrorsOr b) -> t a -> ErrorsOr (t b)
traverseEO f as = wrap $ traverse f' as
  where f' a = case f a of
                 ErrorsOr (Left errs) -> ErrorsAnd errs undefined
                 ErrorsOr (Right b) -> ErrorsAnd [] b
        wrap (ErrorsAnd errs bs) = if null errs then good bs else bad errs

eoMaybe :: Maybe (ErrorsOr a) -> ErrorsOr (Maybe a)
eoMaybe Nothing = good Nothing
eoMaybe (Just eoa) = Just <$> eoa

showStart :: LCRange -> String
showStart (LCRange (LCPos l c) _) = show l ++ ":" ++ show c

reportError :: FilePath -> Ranged String -> IO ()
reportError path (Ranged rng str) =
  hPutStr stderr (path ++ ":" ++ showStart rng ++ ": " ++ str ++ "\n")

reportEO :: FilePath -> ErrorsOr a -> IO a
reportEO path (ErrorsOr (Left errs)) = mapM_ (reportError path) errs >> exitFailure
reportEO _ (ErrorsOr (Right a)) = return a
