module ErrorsOr
  ( ErrorsOr
  , bad , bad1 , good
  , mapEO
  , reportEO
  ) where

import Ranged
import System.Exit
import System.IO

newtype ErrorsOr a = ErrorsOr (Either [Ranged String] a)
  deriving Show

bad :: [Ranged String] -> ErrorsOr a
bad = ErrorsOr . Left

bad1 :: Ranged String -> ErrorsOr a
bad1 rs = ErrorsOr $ Left [rs]

good :: a -> ErrorsOr a
good = ErrorsOr . Right

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

showStart :: LCRange -> String
showStart (LCRange (LCPos l c) _) = show l ++ ":" ++ show c

reportError :: FilePath -> Ranged String -> IO ()
reportError path (Ranged rng str) =
  hPutStr stderr (path ++ ":" ++ showStart rng ++ ": " ++ str ++ "\n")

reportEO :: FilePath -> ErrorsOr a -> IO a
reportEO path (ErrorsOr (Left errs)) = mapM_ (reportError path) errs >> exitFailure
reportEO _ (ErrorsOr (Right a)) = return a

instance Functor ErrorsOr where
  fmap f (ErrorsOr (Left errs)) = ErrorsOr $ Left errs
  fmap f (ErrorsOr (Right a)) = ErrorsOr $ Right $ f a
