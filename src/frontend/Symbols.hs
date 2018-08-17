module Symbols
  ( Module(..)
  , Group(..)
  , Record(..)
  , BitsRecord(..)
  , Expression(..)
  , Atom(..)
  , run
  )
  where

{-
  The symbols pass creates symbol tables nested 3 deep. At the top
  level is a symbol table that maps names to modules. Next, there is a
  symbol table for each module which maps names to ports. Finally,
  there is a symbol table for each group which maps names to record
  statements.
-}

import Control.Applicative
import Control.Monad
import Data.Functor ((<$>))
import qualified Data.Map.Strict as Map
import Data.Array

import ErrorsOr
import Operators
import VInt
import Ranged
import SymbolTable

import qualified Parser as P
import qualified Grouping as G

data Atom = AtomSym Symbol
          | AtomInt VInt

data Expression = ExprAtom Atom
                | ExprParens (Ranged Expression)
                | ExprSel (Ranged Expression)
                  (Ranged Expression) (Maybe (Ranged Expression))
                | ExprConcat [Ranged Expression]
                | ExprReplicate (Ranged Expression) (Ranged Expression)
                | ExprUnOp (Ranged UnOp) (Ranged Expression)
                | ExprBinOp (Ranged BinOp)
                  (Ranged Expression) (Ranged Expression)
                | ExprCond (Ranged Expression)
                  (Ranged Expression) (Ranged Expression)

data Record = Record
              (Ranged Expression)
              (Ranged P.Symbol)
              (Maybe [(Ranged Integer, Ranged Integer)])

data BitsRecord = BitsRecord (Ranged Expression) (Ranged P.Symbol)

data Group = Group
             [Ranged Expression]
             [String]
             (Either [Record] BitsRecord)

type PortSyms = SymbolTable (Maybe (Ranged P.Slice))

data Module = Module { modName :: Ranged P.Symbol
                     , modSyms :: PortSyms
                     , modGrps :: [Group]
                     }

{-
  We start with the problem of generating a symbol table for the ports
  of a module. Because we'll use it a lot, it gets a local type
  synonym.
-}

readPorts :: [Ranged P.Port] -> ErrorsOr PortSyms
readPorts ports = stbToSymbolTable <$> foldEO takePort stbEmpty ports
  where takePort bld rp =
          let P.Port sym slice = rangedData rp in
            stbAdd "port" bld (rangedRange rp) (copyRange rp sym) slice

{-
  Given a PortSyms instance, we can try to make sense of a
  P.Expression. This is just a big structural recursion over the
  syntax.
-}
readAtom :: PortSyms -> LCRange -> P.Atom -> ErrorsOr Expression
readAtom _ _ (P.AtomInt vint) = good $ ExprAtom (AtomInt vint)
readAtom ps rng (P.AtomSym psym) =
  case stLookup psym ps of
    Nothing -> bad1 $ (Ranged rng $ "undeclared symbol: " ++ name ++ ".")
    Just sym -> good $ ExprAtom (AtomSym sym)
  where name = P.symName psym

readParens :: PortSyms -> Ranged P.Expression -> ErrorsOr Expression
readParens ps expr = ExprParens <$> readExpression ps expr

readSel :: PortSyms -> Ranged P.Expression ->
           Ranged P.Expression -> Maybe (Ranged P.Expression) ->
           ErrorsOr Expression
readSel ps var top bot = liftA3 ExprSel (f var) (f top) (eoMaybe (f <$> bot))
  where f = readExpression ps

readConcat :: PortSyms -> [Ranged P.Expression] -> ErrorsOr Expression
readConcat ps exprs = ExprConcat <$> mapEO (readExpression ps) exprs

readReplicate :: PortSyms -> Ranged P.Expression ->
                 Ranged P.Expression -> ErrorsOr Expression
readReplicate ps count val = liftA2 ExprReplicate (read count) (read val)
  where read = readExpression ps

readUnOp :: PortSyms -> Ranged UnOp -> Ranged P.Expression ->
            ErrorsOr Expression
readUnOp ps unop expr = ExprUnOp unop <$> readExpression ps expr

readBinOp :: PortSyms -> Ranged BinOp -> Ranged P.Expression ->
             Ranged P.Expression -> ErrorsOr Expression
readBinOp ps binop e0 e1 = liftA2 (ExprBinOp binop) (read e0) (read e1)
  where read = readExpression ps

readCond :: PortSyms -> Ranged P.Expression -> Ranged P.Expression ->
            Ranged P.Expression -> ErrorsOr Expression
readCond ps e0 e1 e2 = liftA3 ExprCond (read e0) (read e1) (read e2)
  where read = readExpression ps

readExpression' :: PortSyms -> LCRange -> P.Expression -> ErrorsOr Expression
readExpression' ps rng (P.ExprAtom a) = readAtom ps rng a
readExpression' ps _ (P.ExprParens e) = readParens ps e
readExpression' ps _ (P.ExprSel v t b) = readSel ps v t b
readExpression' ps _ (P.ExprConcat es) = readConcat ps es
readExpression' ps _ (P.ExprReplicate c v) = readReplicate ps c v
readExpression' ps _ (P.ExprUnOp uo e) = readUnOp ps uo e
readExpression' ps _ (P.ExprBinOp bo e0 e1) = readBinOp ps bo e0 e1
readExpression' ps _ (P.ExprCond e0 e1 e2) = readCond ps e0 e1 e2

readExpression :: PortSyms -> Ranged P.Expression -> ErrorsOr (Ranged Expression)
readExpression ps rexpr =
  copyRange rexpr <$> readExpression' ps (rangedRange rexpr) (rangedData rexpr)

{-
  Now we have expression parsing, we can switch to the problem of
  making sense of a group of record statements and cover/cross
  statements.
-}
recNameToSym :: STBuilder () -> Ranged P.Symbol -> ErrorsOr (Ranged Symbol)
recNameToSym stb name =
  (copyRange name . fst) <$>
  stbGet "record name" (rangedRange name) (rangedData name) stb

getRecName :: Ranged P.Expression -> Maybe (Ranged P.Symbol) ->
              ErrorsOr (Ranged P.Symbol)
getRecName _ (Just name) = good name
getRecName expr Nothing =
  case rangedData expr of
    P.ExprAtom (P.AtomSym sym) -> good $ copyRange expr sym
    _ -> bad1 $ copyRange expr "Cannot guess a name for recorded expression."

takeRecord :: PortSyms -> (STBuilder (), [Record]) -> G.Record ->
              ErrorsOr (STBuilder (), [Record])

takeRecord ps (stb, recs) (G.Record expr as cover) =
  do { (expr', psym) <- liftA2 (,) (readExpression ps expr) (getRecName expr as)
     ; stb' <- stbAdd "record name" stb (rangedRange psym) psym ()
     ; good $ (stb', Record expr' psym cover : recs)
     }

readBitRecord :: PortSyms -> G.BitsRecord -> ErrorsOr BitsRecord
readBitRecord ps (G.BitsRecord expr name) =
  liftA2 BitsRecord (readExpression ps expr) (getRecName expr name)

readGroup :: PortSyms -> G.Group -> ErrorsOr Group

readGroup ps (G.Group guards scopes (Left recs)) =
  do { (guards', (stb, recs')) <- liftA2 (,)
                                  (mapEO (readExpression ps) guards)
                                  (foldEO (takeRecord ps) (stbEmpty, []) recs)
     ; return $ Group guards' scopes (Left $ reverse recs')
     }

readGroup ps (G.Group guards scopes (Right brec)) =
  do { (guards', brec') <- liftA2 (,)
                           (mapEO (readExpression ps) guards)
                           (readBitRecord ps brec)
     ; return $ Group guards' scopes (Right brec')
     }

readModule :: G.Module -> ErrorsOr Module
readModule (G.Module name ports groups) =
  do { ports' <- readPorts ports
     ; groups' <- mapEO (readGroup ports') groups
     ; good $ Module name ports' groups'
     }

run :: [G.Module] -> ErrorsOr [Module]
run = mapEO readModule
