module Expressions
--  ( run
--  , Expression(..)
--  , Record(..)
--  , Block(..)
--  , Module(..)
--  , Script(..)
--  , Slice(..)
--  , sliceWidth
--  )
where

import Control.Applicative
import Data.Maybe
import qualified Data.Map.Strict as Map

import qualified Symbols as S
import qualified Parser as P
import ErrorsOr
import Operators
import VInt
import Ranged
import SymbolTable

{-
  The expressions pass tightens up our representation of expressions,
  making sure that stuff that looks like it should be an integer
  really is, that bit selects only apply to symbols and so on.
-}
data Expression = ExprSym Symbol
                | ExprInt VInt
                | ExprSel (Ranged Symbol) (Ranged Expression)
                  (Maybe (Ranged Expression))
                | ExprConcat (Ranged Expression) [Ranged Expression]
                | ExprReplicate Int (Ranged Expression)
                | ExprUnOp (Ranged UnOp) (Ranged Expression)
                | ExprBinOp (Ranged BinOp)
                  (Ranged Expression) (Ranged Expression)
                | ExprCond (Ranged Expression)
                  (Ranged Expression) (Ranged Expression)

data Statement = Record (Ranged Expression) (Ranged Symbol) (Maybe P.CoverList)
               | Cross [Ranged Symbol]

data Group = Group (SymbolTable ()) (Maybe (Ranged Expression)) [Statement]

data Slice = Slice Int Int

type PortSyms = SymbolTable (Ranged Slice)

data Module = Module { modName :: Ranged P.Symbol
                     , modSyms :: PortSyms
                     , modGrps :: [Group]
                     }

sliceWidth :: Slice -> Int
sliceWidth (Slice a b) = 1 + (if a < b then b - a else a - b)

tightenAtom :: S.Atom -> Expression
tightenAtom (S.AtomSym sym) = ExprSym sym
tightenAtom (S.AtomInt int) = ExprInt int

tightenSel' :: Ranged S.Expression -> ErrorsOr (Ranged Symbol)
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

tightenRepCount :: Ranged S.Expression -> ErrorsOr Int
tightenRepCount rse =
  case rangedData rse of
    S.ExprAtom (S.AtomInt int) ->
      case checkVInt int $ baseVISchema { viAllowWidth = False
                                        , viAllowSign = False
                                        , viMinValue = Just 0 } of
        Nothing ->
          let i = toInteger int in
            if i < toInteger (minBound :: Int) ||
               i > toInteger (maxBound :: Int) then
              noGood "is out of bounds for an integer."
            else
              good $ fromInteger i
        Just req -> noGood req
    otherwise -> noGood "should be a literal integer."
  where noGood req = bad1 $ copyRange rse ("Replication count " ++ req)

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

tightenStatement :: S.Statement -> ErrorsOr Statement
tightenStatement (S.Record expr name clist) = (\ e -> Record e name clist) <$>
                                              tighten expr
tightenStatement (S.Cross syms) = good $ Cross syms

tightenGroup :: S.Group -> ErrorsOr Group
tightenGroup (S.Group st guard stmts) =
  liftA2 (Group st)
  (eoMaybe (tighten <$> guard)) (mapEO tightenStatement stmts)

tightenBitSel :: P.Symbol -> LCRange -> VInt -> ErrorsOr Integer
tightenBitSel psym rng int =
  case checkVInt int $ baseVISchema { viAllowWidth = False
                                    , viAllowSign = False
                                    , viMinValue = Just 0 } of
    Nothing -> good $ toInteger int
    Just req -> bad1 $ Ranged rng ("Bit selection for port `" ++
                                   P.symName psym ++ "' " ++ req)

tightenSlice :: Ranged P.Symbol -> Maybe (Ranged P.Slice) -> ErrorsOr (Ranged Slice)
tightenSlice rpsym rslice =
  case rslice of
    Nothing -> good $ copyRange rpsym $ Slice 0 0
    Just (Ranged rng (P.Slice va vb)) ->
      do { (a, b) <- liftA2 (,)
                     (tightenBitSel psym rng va) (tightenBitSel psym rng vb)
         ; if abs (a - b) > toInteger (maxBound :: Int) then
             bad1 $ (Ranged rng $
                     "Difference in bit positions overflows an int.")
           else if (- (max (abs a) (abs b))) < toInteger (minBound :: Int) then
             bad1 $ (Ranged rng $
                     "A bit position in the slice overflows an int.")
           else
             good $ Ranged rng $ Slice (fromInteger a) (fromInteger b)
         }
  where psym = rangedData rpsym

tightenPortSyms :: SymbolTable (Maybe (Ranged P.Slice)) ->
                   ErrorsOr (SymbolTable (Ranged Slice))
tightenPortSyms st = eaToEO $ stTraverseWithSym f st
  where f sym a = eoToEA undefined $ tightenSlice sym a

tightenModule :: S.Module -> ErrorsOr Module
tightenModule mod =
  do { (psyms, grps) <- liftA2 (,)
                        (tightenPortSyms (S.modSyms mod))
                        (mapEO tightenGroup (S.modGrps mod))
     ; good $ Module (S.modName mod) psyms grps
     }

run :: [S.Module] -> ErrorsOr [Module]
run = mapEO tightenModule
