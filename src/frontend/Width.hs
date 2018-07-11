module Width
  ( run
  , Script(..)
  , Module(..)
  , ModSymbolTable(..)
  ) where

import Control.Applicative
import Control.Exception.Base
import Data.Array
import Data.Bits
import qualified Data.Map.Strict as Map
import Data.Maybe

import qualified Expressions as E
import qualified Symbols as S
import qualified Parser as P
import ErrorsOr
import Operators
import VInt
import Ranged

{-
  The width pass does width checking (working with a Verilog-like
  semantics but where we don't do magic width promotion based on the
  destination size). The statements unchanged, but we note the width
  of recorded expressions (note the type of mstRecords change gained
  an Int)
-}
data ModSymbolTable = ModSymbolTable { mstMap :: Map.Map String S.ModSymIdx
                                     , mstPorts :: S.SymbolArray (Ranged E.Slice)
                                     , mstTriggers :: S.SymbolArray ()
                                     , mstRMap :: Map.Map String S.Symbol
                                     , mstRecords :: S.SymbolArray Int
                                     }
  deriving Show

data Module = Module ModSymbolTable [E.ModStmt]
  deriving Show

data Script = Script { scrMap :: Map.Map String S.Symbol
                     , scrModules :: S.SymbolArray Module
                     , scrStmts :: [S.TLStmt]
                     }
  deriving Show

symBits :: E.ModSymbolTable -> S.Symbol -> (Int, Int)
symBits mst sym =
  case rangedData (S.symbolData (E.mstPorts mst) sym) of
    E.Slice a b -> (max a b, min a b)

symWidth :: E.ModSymbolTable -> S.Symbol -> Int
symWidth mst sym =
  E.sliceWidth $
  rangedData $
  S.symbolData (E.mstPorts mst) sym

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


checkSel :: E.ModSymbolTable -> Ranged S.Symbol ->
            Maybe (Integer, Integer) -> ErrorsOr ()
checkSel mst rsym used =
  case used of
    -- If we can't figure out the bits in advance, we can't help.
    Nothing -> good ()
    -- If we can, we can check them against the size of the
    -- underlying symbol
    Just (a, b) ->
      let (uhi, ulo) = (max a b, min a b)
          (shi, slo) = symBits mst (rangedData rsym) in
        if ulo < toInteger slo || uhi > toInteger shi then
          bad1 $ copyRange rsym $
          "Bit selection overflows size of symbol."
        else
          good ()

-- TODO: We should support +: and -: so that I can write x[y +: 2] and
-- have a sensible width.
selWidth :: E.ModSymbolTable -> Ranged S.Symbol ->
            Ranged E.Expression -> Maybe (Ranged E.Expression) ->
            ErrorsOr Int
selWidth mst rsym re0 mre1 =
  do { used <- selBits re0 mre1
     ; checkSel mst rsym used
     ; case used of
         Nothing -> bad1 $ copyRange rsym
                    "Can't compute width of bit selection."
         Just (a, b) -> good $ fromInteger $ max a b - min a b + 1
     }

concatWidth :: E.ModSymbolTable -> Ranged E.Expression ->
               [Ranged E.Expression] -> ErrorsOr Int
concatWidth mst re res =
  do { (w0, ws) <- liftA2 (,) (get re) (mapEO get res)
     ; good $ w0 + sum ws
     }
  where get = exprWidth mst

repWidth :: E.ModSymbolTable -> Int ->
            Ranged E.Expression -> ErrorsOr Int
repWidth mst n re = ((*) n) <$> (exprWidth mst re)

unOpWidth :: E.ModSymbolTable ->
             Ranged UnOp -> Ranged E.Expression -> ErrorsOr Int
unOpWidth mst uo re =
  do { ew <- exprWidth mst re
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

binOpWidth :: E.ModSymbolTable ->
              Ranged BinOp -> Ranged E.Expression -> Ranged E.Expression ->
              ErrorsOr Int
binOpWidth mst bo re0 re1 =
  do { (ew0, ew1) <- liftA2 (,) (exprWidth mst re0) (exprWidth mst re1)
     ; checkWidths (show $ rangedData bo) (rangedRange bo) ew0 ew1
     ; good $ if binOpIsReduction (rangedData bo) then 1 else ew0
     }

condWidth :: E.ModSymbolTable -> Ranged E.Expression -> Ranged E.Expression ->
             Ranged E.Expression -> ErrorsOr Int
condWidth mst e0 e1 e2 =
  do { (ew0, ew1, ew2) <- liftA3 (,,) (get e0) (get e1) (get e2)
     ; liftA2 (,) (checkWidth1 (rangedRange e0) ew0)
                  (checkWidths "conditional" (rangedRange e1) ew1 ew2)
     ; return ew1
     }
  where get = exprWidth mst

exprWidth :: E.ModSymbolTable -> Ranged E.Expression -> ErrorsOr Int
exprWidth mst rexpr = exprWidth' mst (rangedRange rexpr) (rangedData rexpr)

exprWidth' :: E.ModSymbolTable -> LCRange -> E.Expression -> ErrorsOr Int
exprWidth' mst _ (E.ExprSym sym) = return $ symWidth mst sym
exprWidth' _ rng (E.ExprInt vint) = intWidth rng vint
exprWidth' mst _ (E.ExprSel sym ex0 ex1) = selWidth mst sym ex0 ex1
exprWidth' mst _ (E.ExprConcat e0 es) = concatWidth mst e0 es
exprWidth' mst _ (E.ExprReplicate n e) = repWidth mst n e
exprWidth' mst _ (E.ExprUnOp uo e) = unOpWidth mst uo e
exprWidth' mst _ (E.ExprBinOp bo e0 e1) = binOpWidth mst bo e0 e1
exprWidth' mst _ (E.ExprCond e0 e1 e2) = condWidth mst e0 e1 e2

takeAssign :: E.ModSymbolTable -> Ranged S.Symbol -> Ranged E.Expression ->
              ErrorsOr ()
takeAssign mst sym expr =
  do { w <- exprWidth mst expr
     ; if w /= 1 then
         bad1 $ copyRange sym $
         "Assignment to trigger `" ++ show (rangedData sym) ++
         "' has width " ++ show w ++ " != 1."
       else
         good ()
     }

takeRecord :: E.ModSymbolTable -> Map.Map Int Int -> Maybe (Ranged S.Symbol) ->
              Ranged E.Expression -> Ranged S.Symbol ->
              ErrorsOr (Map.Map Int Int)
takeRecord mst m guard expr name =
  do { w <- exprWidth mst expr
     ; assert (not $ Map.member idx m) $
       good $ Map.insert idx w m
     }
  where idx = S.symbolIdx $ rangedData name

takeModStmt :: E.ModSymbolTable -> Map.Map Int Int -> E.ModStmt ->
               ErrorsOr (Map.Map Int Int)
takeModStmt mst m (E.Assign lhs rhs) = takeAssign mst lhs rhs >> good m
takeModStmt mst m (E.Record guard expr name) = takeRecord mst m guard expr name

takeModStmts :: E.ModSymbolTable -> [E.ModStmt] ->
                ErrorsOr (Map.Map Int Int)
takeModStmts mst = foldEO (takeModStmt mst) Map.empty

buildMSTRecords :: S.SymbolArray () -> Map.Map Int Int -> S.SymbolArray Int
buildMSTRecords (S.SymbolArray recs) widths =
  S.SymbolArray $
  listArray (bounds recs) [ upd i (recs ! i) | i <- indices recs ]
  where upd i (S.SymbolEntry sym ()) =
          S.SymbolEntry sym (fromJust (Map.lookup i widths))

buildMST :: E.ModSymbolTable -> Map.Map Int Int ->
            ModSymbolTable
buildMST mst m = ModSymbolTable
                 (E.mstMap mst)
                 (E.mstPorts mst)
                 (E.mstTriggers mst)
                 (E.mstRMap mst) $
                 buildMSTRecords (E.mstRecords mst) m

takeModule :: E.Module -> ErrorsOr Module
takeModule (E.Module mst stmts) =
  do { widths <- takeModStmts mst stmts
     ; return $ Module (buildMST mst widths) stmts
     }

takeModules :: S.SymbolArray E.Module -> ErrorsOr (S.SymbolArray Module)
takeModules (S.SymbolArray mods) =
  S.SymbolArray <$> amapEO (S.mapSE takeModule) mods

fitsInBits :: Integer -> Int -> Bool
fitsInBits n w = assert (w > 0) $ shift (abs n) (- sw) == 0
  where sw = if n >= 0 then w else w - 1

checkCover1 :: Int -> Ranged VInt -> ErrorsOr ()
checkCover1 w rint =
  if fitsInBits asInt w then good ()
  else bad1 $ copyRange rint $
       "Cover list has entry of " ++ show asInt ++
       ", but the cover expression has width " ++ show w ++ "."
  where asInt = toInteger (rangedData rint)

checkCover' :: LCRange -> Int -> Maybe P.CoverList -> ErrorsOr ()
checkCover' rng w Nothing =
  if w > 16 then
    bad1 $ Ranged rng "Symbol has width more than 16 and no cover list."
  else
    good ()

checkCover' _ w (Just (P.CoverList vints)) =
  mapEO (checkCover1 w) vints >> good ()

recWidth :: ModSymbolTable -> S.Symbol -> Int
recWidth mst sym = S.symbolData (mstRecords mst) sym

checkCover :: Map.Map String S.Symbol ->
              S.SymbolArray Module ->
              Ranged S.DottedSymbol -> Maybe P.CoverList ->
              ErrorsOr ()
checkCover modMap mods dsym clist =
  checkCover' (rangedRange dsym) (recWidth mst vsym) clist
  where S.DottedSymbol msym vsym = rangedData dsym
        Module mst _ = S.symbolData mods msym

checkTLStmt :: Map.Map String S.Symbol -> S.SymbolArray Module ->
               S.TLStmt -> ErrorsOr ()
checkTLStmt modMap mods (S.Cover dsym cov) = checkCover modMap mods dsym cov
checkTLStmt _ _ (S.Cross _) = good ()

checkTLStmts :: Map.Map String S.Symbol -> S.SymbolArray Module ->
                [S.TLStmt] -> ErrorsOr ()
checkTLStmts modMap mods = foldEO (\ _ -> checkTLStmt modMap mods) ()

run :: E.Script -> ErrorsOr Script
run script = do { mods <- takeModules (E.scrModules script)
                ; checkTLStmts smap mods stmts
                ; good $ Script smap mods stmts
                }
  where smap = E.scrMap script
        stmts = E.scrStmts script
