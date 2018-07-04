module VInt
  ( VInt
  , makeVInt , basicVInt
  , vIntWidth
  , VISchema(..) , baseVISchema
  , checkVInt
  ) where

import Data.Bits
import Data.Maybe

data VInt = VInt (Maybe Int) Bool Integer
  deriving (Eq, Show)

basicVInt :: Integer -> VInt
basicVInt n = VInt Nothing False n

vIntValue :: VInt -> Integer
vIntValue (VInt _ _ n) = n

vIntWidth :: VInt -> Maybe Int
vIntWidth (VInt w _ _) = w

makeVInt :: Maybe Integer -> Bool -> Integer -> Either String VInt
makeVInt Nothing sgn n = Right $ VInt Nothing sgn n
makeVInt (Just w) sgn n =
  if w < 1 then
    Left "Integer width must be positive."
  else if sgn && w == 1 then
    Left "Cannot form a 1-bit signed integer."
  else if w > toInteger (maxBound :: Int) then
    Left "Implausibly large width."
  else if shiftR (abs n) bitsAvailable /= 0 then
    Left $ "Integer literal doesn't fit in the given width."
  else
    Right $ VInt (Just intw) sgn n
  where intw = fromInteger w
        bitsAvailable = if sgn then intw - 1 else intw

data VISchema = VISchema { viRequireWidth :: Bool
                         , viAllowWidth   :: Bool
                         , viAllowSign    :: Bool
                         , viMinValue     :: Maybe Integer
                         }

baseVISchema :: VISchema
baseVISchema = VISchema False True True Nothing

checkMinValue :: Maybe Integer -> Integer -> Maybe String
checkMinValue Nothing _ = Nothing
checkMinValue (Just m) n =
  if n < m then
    Just $ "must be at least " ++ show m ++ "."
  else
    Nothing

checkVInt :: VInt -> VISchema -> Maybe String
checkVInt (VInt w sgn n) schema =
  if viRequireWidth schema && isNothing w then
    Just $ "needs a width."
  else if isJust w && not (viAllowWidth schema) then
    Just $ "shouldn't have a width."
  else if sgn && not (viAllowSign schema) then
    Just $ "may not be signed."
  else
    checkMinValue (viMinValue schema) n

makeCombinedVInt :: VInt -> VInt -> Integer -> VInt
makeCombinedVInt (VInt (Just w0) sgn0 n0) (VInt (Just w1) sgn1 n1) =
  VInt (Just (max w0 w1)) (sgn0 || sgn1)
makeCombinedVInt (VInt (Just w0) sgn0 n0) (VInt Nothing sgn1 n1) =
  VInt (Just w0) (sgn0 || sgn1)
makeCombinedVInt (VInt Nothing sgn0 n0) (VInt (Just w1) sgn1 n1) =
  VInt (Just w1) (sgn0 || sgn1)
makeCombinedVInt (VInt Nothing sgn0 n0) (VInt Nothing sgn1 n1) =
  VInt Nothing (sgn0 || sgn1)

vIntQuotRem :: VInt -> VInt -> (VInt, VInt)
vIntQuotRem v0 v1 = (mkQR q, mkQR r)
  where (q, r) = quotRem (vIntValue v0) (vIntValue v1)
        mkQR = makeCombinedVInt v0 v1

liftUnOp :: (Integer -> Integer) -> VInt -> VInt
liftUnOp op (VInt w sgn n) = VInt w sgn (op n)

liftBinOp :: (Integer -> Integer -> Integer) ->
             VInt -> VInt -> VInt
liftBinOp op a b =
  makeCombinedVInt a b (vIntValue a `op` vIntValue b)

instance Ord VInt where
  -- Note: This is a bit naughty because it isn't a total order.
  compare (VInt _ _ a) (VInt _ _ b) = compare a b

instance Num VInt where
  (+) = liftBinOp (+)
  (*) = liftBinOp (*)
  abs = liftUnOp abs
  signum = liftUnOp signum
  fromInteger = basicVInt
  negate = liftUnOp negate

instance Real VInt where
  toRational = toRational . vIntValue

instance Enum VInt where
  toEnum = basicVInt . toInteger
  fromEnum = fromInteger . vIntValue

instance Integral VInt where
  quotRem = vIntQuotRem
  toInteger = vIntValue

