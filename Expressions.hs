module Expressions
  ( run
  , Expression(..)
  , ModSymbolTable(..)
  , Slice(..)
  , sliceWidth
  ) where

import Control.Applicative
import Data.Maybe
import qualified Data.Map.Strict as Map

import qualified Symbols as S
import qualified Parser as P
import ErrorsOr
import Operators
import Ranged

{-
  The expressions pass tightens up our representation of expressions,
  making sure that stuff that looks like it should be an integer
  really is, that bit selects only apply to symbols and so on.
-}
data Expression = ExprSym S.Symbol
                | ExprInt P.VInt
                | ExprSel (Ranged S.Symbol) (Ranged Expression)
                  (Maybe (Ranged Expression))
                | ExprConcat (Ranged Expression) [Ranged Expression]
                | ExprReplicate Integer (Ranged Expression)
                | ExprUnOp (Ranged UnOp) (Ranged Expression)
                | ExprBinOp (Ranged BinOp)
                  (Ranged Expression) (Ranged Expression)
                | ExprCond (Ranged Expression)
                  (Ranged Expression) (Ranged Expression)
  deriving Show

data ModStmt = Assign (Ranged S.Symbol) (Ranged Expression)
             | Record (Maybe (Ranged S.Symbol))
               (Ranged Expression) (Ranged S.Symbol)
  deriving Show

data Slice = Slice Integer Integer
  deriving Show

sliceWidth :: Slice -> Integer
sliceWidth (Slice a b) = 1 + (if a < b then b - a else a - b)

data ModSymbolTable = ModSymbolTable { mstMap :: Map.Map String S.ModSymIdx
                                     , mstPorts :: S.SymbolArray (Ranged Slice)
                                     , mstTriggers :: S.SymbolArray ()
                                     , mstRMap :: Map.Map String S.Symbol
                                     , mstRecords :: S.SymbolArray ()
                                     }
  deriving Show

data Module = Module ModSymbolTable [ModStmt]
  deriving Show

data Script = Script { scrMap :: Map.Map String S.Symbol
                     , scrModules :: S.SymbolArray Module
                     , scrStmts :: [S.TLStmt]
                     }
  deriving Show

tightenAtom :: S.Atom -> Expression
tightenAtom (S.AtomSym sym) = ExprSym sym
tightenAtom (S.AtomInt int) = ExprInt int

tightenSel' :: Ranged S.Expression -> ErrorsOr (Ranged S.Symbol)
tightenSel' rse =
  case rangedData rse of
    S.ExprAtom (S.AtomSym sym) -> good $ copyRange rse sym
    _ -> bad1 (copyRange rse
               "bit selection applied to something other than a symbol.")

tightenSel :: Ranged S.Expression -> Ranged S.Expression ->
              Maybe (Ranged S.Expression) -> ErrorsOr Expression
tightenSel base top bot =
  liftA3 ExprSel (tightenSel' base) (tighten top) (eoMaybe $ tighten <$> bot)

tightenConcat :: LCRange -> [Ranged S.Expression] -> ErrorsOr Expression
tightenConcat rng [] = bad1 $ Ranged rng "Empty concatenation."
tightenConcat rng (e : es) =
  liftA2 ExprConcat (tighten e) (mapEO tighten es)

tightenRepCount :: Ranged S.Expression -> ErrorsOr Integer
tightenRepCount rse =
  case rangedData rse of
    S.ExprAtom (S.AtomInt (P.VInt width signed num)) ->
      if isJust width then noGood "Replication count can't have a width."
      else if signed then noGood "Replication count shouldn't be signed."
      else if num < 0 then noGood "Replication count shouldn't be negative."
      else good num
    otherwise ->
      noGood "Replication count should be a literal integer."
  where noGood msg = bad1 $ copyRange rse msg

tightenReplicate :: Ranged S.Expression -> Ranged S.Expression ->
                    ErrorsOr Expression
tightenReplicate count val =
  liftA2 ExprReplicate (tightenRepCount count) (tighten val)

tightenUnOp :: Ranged UnOp -> Ranged S.Expression -> ErrorsOr Expression
tightenUnOp uo se = ExprUnOp uo <$> tighten se

tightenBinOp :: Ranged BinOp -> Ranged S.Expression -> Ranged S.Expression ->
                ErrorsOr Expression
tightenBinOp bo se0 se1 = liftA2 (ExprBinOp bo) (tighten se0) (tighten se1)

tightenCond :: Ranged S.Expression -> Ranged S.Expression ->
               Ranged S.Expression -> ErrorsOr Expression
tightenCond se0 se1 se2 =
  liftA3 ExprCond (tighten se0) (tighten se1) (tighten se2)

tighten' :: LCRange -> S.Expression -> ErrorsOr Expression
tighten' _ (S.ExprAtom atom) = good $ tightenAtom atom
tighten' _ (S.ExprParens se) = rangedData <$> tighten se
tighten' _ (S.ExprSel base top bot) = tightenSel base top bot
tighten' r (S.ExprConcat exprs) = tightenConcat r exprs
tighten' _ (S.ExprReplicate count val) = tightenReplicate count val
tighten' _ (S.ExprUnOp uo se) = tightenUnOp uo se
tighten' _ (S.ExprBinOp bo se0 se1) = tightenBinOp bo se0 se1
tighten' _ (S.ExprCond se0 se1 se2) = tightenCond se0 se1 se2

tighten :: Ranged S.Expression -> ErrorsOr (Ranged Expression)
tighten rse = copyRange rse <$> tighten' (rangedRange rse) (rangedData rse)

tightenModStmt :: S.ModStmt -> ErrorsOr ModStmt
tightenModStmt (S.Assign lhs rhs) = Assign lhs <$> tighten rhs
tightenModStmt (S.Record guard expr dest) =
  (\ e -> Record guard e dest) <$> tighten expr

tightenBitSel :: P.Symbol -> LCRange -> P.VInt -> ErrorsOr Integer
tightenBitSel psym rng (P.VInt width signed num) =
  if isJust width then noGood " has a width."
  else if signed then noGood " is explicitly signed."
  else if num < 0 then noGood " is negative."
  else good num
  where noGood msg =
          bad1 $ Ranged rng ("Bit selection for port `" ++ P.symName psym ++
                             "': " ++ msg)

tightenSlice :: P.Symbol -> Ranged (Maybe P.Slice) -> ErrorsOr (Ranged Slice)
tightenSlice psym rslice =
  copyRange rslice <$>
  case rangedData rslice of
    Nothing -> good $ Slice 0 0
    Just (P.Slice va vb) ->
      liftA2 Slice (tightenBitSel psym rng va) (tightenBitSel psym rng vb)
  where rng = rangedRange rslice

tightenSlices :: S.SymbolArray (Ranged (Maybe P.Slice)) ->
                 ErrorsOr (S.SymbolArray (Ranged Slice))
tightenSlices (S.SymbolArray slices) =
  S.SymbolArray <$> amapEO (S.mapSE' tightenSlice) slices

tightenMST :: S.ModSymbolTable -> ErrorsOr ModSymbolTable
tightenMST smst =
  do { slices <- tightenSlices (S.mstPorts smst)
     ; return $
       ModSymbolTable (S.mstMap smst) slices (S.mstTriggers smst)
                      (S.mstRMap smst) (S.mstRecords smst)
     }

tightenModule :: S.Module -> ErrorsOr Module
tightenModule (S.Module mst stmts) =
  liftA2 Module (tightenMST mst) (mapEO tightenModStmt stmts)

tightenModules :: S.SymbolArray S.Module -> ErrorsOr (S.SymbolArray Module)
tightenModules (S.SymbolArray modules) =
  S.SymbolArray <$> amapEO (S.mapSE tightenModule) modules

run :: S.Script -> ErrorsOr Script
run s = do { mods <- tightenModules $ S.scrModules s
           ; return $ Script (S.scrMap s) mods (S.scrStmts s)
           }
