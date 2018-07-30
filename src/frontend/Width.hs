{-# LANGUAGE DeriveGeneric #-}

module Width
  ( run
  , Record(..)
  , BitsRecord(..)
  , Group(..)
  , grpWidth
  , grpExprs
  , grpGuards
  , Module(..)
  ) where

import Control.Applicative
import Control.Exception.Base
import Data.Bits
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Hashable
import GHC.Generics (Generic)

import BuildVersion (gitDescribe)
import qualified Expressions as E
import qualified Symbols as S
import qualified Parser as P
import SymbolTable
import ErrorsOr
import Operators
import VInt
import Ranged
import RangeList

{-
  The width pass does width checking (working with a Verilog-like
  semantics but where we don't do magic width promotion based on the
  destination size). The statements remain unchanged, but we note the
  width of recorded expressions.
-}
data Record = Record { recExpr :: Ranged E.Expression
                     , recSym :: Ranged P.Symbol
                     , recClist :: RangeList
                     , recWidth :: Int
                     }
  deriving Generic

instance Hashable Record

data BitsRecord = BitsRecord { brExpr :: Ranged E.Expression
                             , brSym :: Ranged P.Symbol
                             , brWidth :: Int
                             }
  deriving Generic

instance Hashable BitsRecord

data Group = Group
             [Ranged E.Expression]
             (Either [Record] BitsRecord)
  deriving Generic

instance Hashable Group

grpWidth :: Group -> Int
grpWidth (Group _ (Left recs)) = sum $ map recWidth recs
grpWidth (Group _ (Right brec)) = brWidth brec

grpExprs :: Group -> [Ranged E.Expression]
grpExprs (Group _ (Left recs)) = map recExpr recs
grpExprs (Group _ (Right brec)) = [brExpr brec]

grpGuards :: Group -> [Ranged E.Expression]
grpGuards (Group guards _) = guards

data Module = Module { modName :: Ranged P.Symbol
                     , modSyms :: SymbolTable (Ranged E.Slice)
                     , modGroups :: [Group]
                     }
  deriving Generic

instance Hashable Module

symBits :: SymbolTable (Ranged E.Slice) -> Symbol -> (Int, Int)
symBits st sym =
  let E.Slice a b = rangedData (stAt sym st) in (max a b, min a b)

symWidth :: SymbolTable (Ranged E.Slice) -> Symbol -> Int
symWidth st sym = E.sliceWidth $ rangedData $ stAt sym st

intWidth :: LCRange -> VInt -> ErrorsOr Int
intWidth rng n =
  case vIntWidth n of
    Just w -> good w
    Nothing ->
      bad1 $ Ranged rng "Integer with no width used in expression."

{-
  Try to interpret the expression as an integer. If we can't figure
  out the answer, return Nothing. If something is properly wrong,
  return an error.
-}
exprAsVInt :: E.Expression -> ErrorsOr (Maybe VInt)
exprAsVInt (E.ExprSym _) = good $ Nothing
exprAsVInt (E.ExprInt vi) = good $ Just vi
exprAsVInt (E.ExprSel _ _ _) = good $ Nothing
exprAsVInt (E.ExprConcat _ _) = good $ Nothing
exprAsVInt (E.ExprReplicate _ _) = good $ Nothing
exprAsVInt (E.ExprUnOp ruo re) =
  do { expr <- get re
     ; case expr of
         Nothing -> good $ Nothing
         Just expr' ->
           case applyUnOp (rangedData ruo) expr' of
             Left msg -> bad1 $ copyRange ruo $
                         "Cannot apply unary operator: " ++ msg
             Right val -> good $ Just val
     }
  where get = exprAsVInt . rangedData

exprAsVInt (E.ExprBinOp rbo re0 re1) =
  do { (e0, e1) <- liftA2 (,) (get re0) (get re1)
     ; if isNothing e0 || isNothing e1 then
         good $ Nothing
       else
         let Just e0' = e0 ; Just e1' = e1 in
           case applyBinOp (rangedData rbo) e0' e1' of
             Left msg -> bad1 $ copyRange rbo $
                         "Cannot apply binary operator: " ++ msg
             Right val -> good $ Just val
     }
  where get = exprAsVInt . rangedData

exprAsVInt (E.ExprCond a b c) =
  do { (ea, eb, ec) <- liftA3 (,,) (get a) (get b) (get c)
     ; if isNothing ea || isNothing eb || isNothing ec then
         good $ Nothing
       else
         let Just ea' = ea ; Just eb' = eb ; Just ec' = ec in
           case applyCond ea' eb' ec' of
             Left msg -> bad1 $ copyRange a $
                         "Cannot apply conditional: " ++ msg
             Right val -> good $ Just val
     }
  where get = exprAsVInt . rangedData

selBits :: Ranged E.Expression -> Maybe (Ranged E.Expression) ->
           ErrorsOr (Maybe (Integer, Integer))
selBits re0 mre1 =
   case mre1 of
     Nothing ->
       do { mv0 <- get re0
          ; good $
            case mv0 of
              Nothing -> Nothing
              Just v0 -> Just (toInteger v0, toInteger v0)
          }
     Just re1 ->
       do { (mv0, mv1) <- liftA2 (,) (get re0) (get re1)
          ; good $
            if isNothing mv0 || isNothing mv1 then
              Nothing
            else
              Just (toInteger $ fromJust mv0, toInteger $ fromJust mv1)
          }
  where get = exprAsVInt . rangedData


checkSel :: SymbolTable (Ranged E.Slice) -> Ranged Symbol ->
            Maybe (Integer, Integer) -> ErrorsOr ()
checkSel st rsym used =
  case used of
    -- If we can't figure out the bits in advance, we can't help.
    Nothing -> good ()
    -- If we can, we can check them against the size of the
    -- underlying symbol
    Just (a, b) ->
      let (uhi, ulo) = (max a b, min a b)
          (shi, slo) = symBits st (rangedData rsym) in
        if ulo < toInteger slo || uhi > toInteger shi then
          bad1 $ copyRange rsym $
          "Bit selection overflows size of symbol."
        else
          good ()

-- TODO: We should support +: and -: so that I can write x[y +: 2] and
-- have a sensible width.
selWidth :: SymbolTable (Ranged E.Slice) -> Ranged Symbol ->
            Ranged E.Expression -> Maybe (Ranged E.Expression) ->
            ErrorsOr Int
selWidth st rsym re0 mre1 =
  do { used <- selBits re0 mre1
     ; checkSel st rsym used
     ; case used of
         Nothing -> bad1 $ copyRange rsym
                    "Can't compute width of bit selection."
         Just (a, b) -> good $ fromInteger $ max a b - min a b + 1
     }

concatWidth :: SymbolTable (Ranged E.Slice) -> Ranged E.Expression ->
               [Ranged E.Expression] -> ErrorsOr Int
concatWidth st re res =
  do { (w0, ws) <- liftA2 (,) (get re) (mapEO get res)
     ; good $ w0 + sum ws
     }
  where get = exprWidth st

repWidth :: SymbolTable (Ranged E.Slice) -> Int ->
            Ranged E.Expression -> ErrorsOr Int
repWidth st n re = ((*) n) <$> (exprWidth st re)

unOpWidth :: SymbolTable (Ranged E.Slice) ->
             Ranged UnOp -> Ranged E.Expression -> ErrorsOr Int
unOpWidth st uo re =
  do { ew <- exprWidth st re
     ; return $ if unOpIsReduction (rangedData uo) then 1 else ew
     }

checkWidth1 :: LCRange -> Int -> ErrorsOr ()
checkWidth1 rng n =
  if n /= 1 then
    bad1 $ Ranged rng $
    "Expression has width " ++ show n ++
    " != 1 so can't be used as a condition."
  else
    good ()

checkWidths :: String -> LCRange -> Int -> Int -> ErrorsOr ()
checkWidths opname rng n m =
  if n /= m then
    bad1 $ Ranged rng $
    "Left and right side of " ++ opname ++
    " operator have different widths: " ++
    show n ++ " != " ++ show m ++ "."
  else
    good ()

binOpWidth :: SymbolTable (Ranged E.Slice) ->
              Ranged BinOp -> Ranged E.Expression -> Ranged E.Expression ->
              ErrorsOr Int
binOpWidth st bo re0 re1 =
  do { (ew0, ew1) <- liftA2 (,) (exprWidth st re0) (exprWidth st re1)
     ; checkWidths (show $ rangedData bo) (rangedRange bo) ew0 ew1
     ; good $ if binOpIsReduction (rangedData bo) then 1 else ew0
     }

condWidth :: SymbolTable (Ranged E.Slice) -> Ranged E.Expression -> Ranged E.Expression ->
             Ranged E.Expression -> ErrorsOr Int
condWidth st e0 e1 e2 =
  do { (ew0, ew1, ew2) <- liftA3 (,,) (get e0) (get e1) (get e2)
     ; liftA2 (,) (checkWidth1 (rangedRange e0) ew0)
                  (checkWidths "conditional" (rangedRange e1) ew1 ew2)
     ; return ew1
     }
  where get = exprWidth st

exprWidth :: SymbolTable (Ranged E.Slice) -> Ranged E.Expression -> ErrorsOr Int
exprWidth st rexpr = exprWidth' st (rangedRange rexpr) (rangedData rexpr)

exprWidth' :: SymbolTable (Ranged E.Slice) -> LCRange -> E.Expression -> ErrorsOr Int
exprWidth' st _ (E.ExprSym sym) = return $ symWidth st sym
exprWidth' _ rng (E.ExprInt vint) = intWidth rng vint
exprWidth' st _ (E.ExprSel sym ex0 ex1) = selWidth st sym ex0 ex1
exprWidth' st _ (E.ExprConcat e0 es) = concatWidth st e0 es
exprWidth' st _ (E.ExprReplicate n e) = repWidth st n e
exprWidth' st _ (E.ExprUnOp uo e) = unOpWidth st uo e
exprWidth' st _ (E.ExprBinOp bo e0 e1) = binOpWidth st bo e0 e1
exprWidth' st _ (E.ExprCond e0 e1 e2) = condWidth st e0 e1 e2

fitsInBits :: Integer -> Int -> Bool
fitsInBits n w = assert (w > 0) $ shift (abs n) (- sw) == 0
  where sw = if n >= 0 then w else w - 1

makeCList :: LCRange -> Int ->
             Maybe [(Ranged Integer, Ranged Integer)] -> ErrorsOr RangeList
makeCList rng w Nothing =
  if w > 16 then
    bad1 $ Ranged rng
    "Record has width more than 16 and no cover list."
  else
    return $ rlRange (0, max)
  where max = (shiftL (1 :: Integer) w) - 1

makeCList _ w (Just pairs) = foldEO f rlEmpty pairs
  where f rl (lo, hi) =
          do { chkRng lo
             ; chkRng hi
             ; if rangedData hi < rangedData lo then
                 bad1 $ copyRange hi $
                 "Cover list has a range with min less than max."
               else
                 good $ rlAdd (rangedData lo, rangedData hi) rl
             }
        chkRng x =
          if (not $ fitsInBits (rangedData x) w) then
            bad1 $ copyRange x $
            "Cover list has entry of " ++ show (rangedData x) ++
            ", but the cover expression has width " ++ show w ++ "."
          else
            return ()

takeRec :: SymbolTable (Ranged E.Slice) -> E.Record -> ErrorsOr Record
takeRec st (E.Record expr rsym clist) =
  do { w <- exprWidth st expr
     ; clist' <- makeCList (rangedRange expr) w clist
     ; good $ Record expr rsym clist' w
     }

checkGuard :: SymbolTable (Ranged E.Slice) -> Ranged E.Expression -> ErrorsOr ()
checkGuard symST guard =
  do { gw <- exprWidth symST guard
     ; if gw /= 1 then
         bad1 $ copyRange guard $
         "Block is guarded by expression with width " ++ show gw ++ ", not 1."
       else
         good ()
     }

readGroup :: SymbolTable (Ranged E.Slice) -> E.Group -> ErrorsOr Group
readGroup symST (E.Group guards (Left recs)) =
  do { recs' <- snd <$> (liftA2 (,)
                         (mapEO (checkGuard symST) guards)
                         (mapEO (takeRec symST) recs))
     ; return $ Group guards $ Left recs'
     }

readGroup symST (E.Group guards (Right (E.BitsRecord expr name))) =
  do { w <- snd <$> (liftA2 (,)
                      (mapEO (checkGuard symST) guards)
                      (exprWidth symST expr))
     ; return $ Group guards $ Right $ BitsRecord expr name w
     }

readModule :: E.Module -> ErrorsOr Module
readModule mod =
  Module (E.modName mod) (E.modSyms mod) <$>
  mapEO (readGroup (E.modSyms mod)) (E.modGrps mod)

run :: [E.Module] -> ErrorsOr (Int, [Module])
run mods = do { mods' <- mapEO readModule mods
              ; return (hash (mods', gitDescribe), mods')
              }
