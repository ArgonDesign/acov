module Symbols
  ( Script(..)
  , TLStmt(..)
  , Module(..)
  , Block(..)
  , Record(..)
  , Expression(..)
  , Atom(..)
  , DottedSymbol(..)
  , run
  )
  where

{-
  The symbols pass creates a global symbol table for storing module
  names and a symbol table for each module. Things that were P.Symbol
  become Symbol (exported from this module), which is fundamentally an
  integer. Things that were P.DottedSymbol become DottedSymbol (also
  exported from this module), which is a pair of integers: the first
  identifies the module and the second identifies the symbol within
  it.
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

data DottedSymbol = DottedSymbol Symbol Symbol

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

data Record = Record (Ranged Expression) Symbol

data Block = Block (Maybe (Ranged Expression)) [Record]

data TLStmt = Cover (Ranged DottedSymbol) (Maybe P.CoverList)
            | Cross [Ranged DottedSymbol]

{-
  PortSyms is the symbol table for ports of a module.
-}
type PortSyms = SymbolTable (Maybe (Ranged P.Slice))

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
  We want to build up a module instance. This will contain a PortSyms
  for the input ports, together with a symbol table for record
  statements and a list of blocks. While building this list of blocks,
  we'll generate the symbol table for record statements.
-}
recordName :: P.Record -> ErrorsOr (Ranged P.Symbol)
recordName (P.Record _ (Just rsym)) = good rsym
recordName (P.Record expr Nothing) =
  case rangedData expr of
    P.ExprAtom (P.AtomSym sym) -> good $ copyRange expr $ sym
    _ -> (bad1 $ copyRange expr
           "Cannot guess a name for recorded expression.")


takeRecord :: PortSyms -> (STBuilder (), [Record]) -> P.Record ->
              ErrorsOr (STBuilder (), [Record])
takeRecord ps (stb, rs) record =
  do { recname <- recordName record
     ; (stb', expr') <- liftA2 (,)
                        (stbAdd "record" stb (rangedRange expr) recname ())
                        (readExpression ps expr)
     ; return $ (stb', Record expr' (stbLastSymbol stb') : rs)
     }
  where P.Record expr _ = record
        addRec x = x : rs


takeBlock :: PortSyms -> (STBuilder (), [Block]) -> P.Block ->
             ErrorsOr (STBuilder (), [Block])
takeBlock ps (stb, blocks) (P.Block mre records) =
  do { (guard, (stb', records')) <- liftA2 (,)
                                    (readMaybe mre)
                                    (foldEO (takeRecord ps) (stb, []) records)
     ; return $ (stb', Block guard (reverse records') : blocks)
     }
  where readMaybe Nothing = return Nothing
        readMaybe (Just e) = Just <$> readExpression ps e


data Module = Module { modSyms :: PortSyms
                     , modRecs :: SymbolTable ()
                     , modBlocks :: [Block]
                     }

readModule :: [Ranged P.Port] -> [P.Block] -> ErrorsOr Module
readModule ports pblocks =
  do { ps <- readPorts ports
     ; (stb, blocks) <- foldEO (takeBlock ps) (stbEmpty, []) pblocks
     ; return $ Module ps (stbToSymbolTable stb) (reverse blocks)
     }

takeModule :: STBuilder Module ->
              Ranged P.Symbol -> [Ranged P.Port] -> [P.Block] ->
              ErrorsOr (STBuilder Module)
takeModule stb name ports blocks =
  readModule ports blocks >>= stbAdd "module" stb (rangedRange name) name

readDottedSymbol :: STBuilder Module -> Ranged P.DottedSymbol ->
                    ErrorsOr (Ranged DottedSymbol)
readDottedSymbol stb rds =
  do { (sym, mod) <- stbGet "module" rng mname stb
     ; record <- fst <$> stGet "record" rng sname (modRecs mod)
     ; return $ copyRange rds (DottedSymbol sym record)
     }
   where P.DottedSymbol mname sname = rangedData rds
         rng = rangedRange rds

readCover :: STBuilder Module -> Ranged P.DottedSymbol -> Maybe P.CoverList ->
             ErrorsOr TLStmt
readCover stb rpds coverlist =
  do { rds <- readDottedSymbol stb rpds
     ; return $ Cover rds coverlist
     }

readCross :: STBuilder Module -> [Ranged P.DottedSymbol] -> ErrorsOr TLStmt
readCross stb lst = Cross <$> mapEO (readDottedSymbol stb) lst

takeTLStmt :: (STBuilder Module, [TLStmt]) -> P.TLStmt ->
              ErrorsOr (STBuilder Module, [TLStmt])

takeTLStmt (stb, stmts) (P.Module name ports blocks) =
  do { stb' <- takeModule stb name ports blocks
     ; return (stb', stmts)
     }

takeTLStmt (stb, stmts) (P.Cover name clist) =
  do { stmt <- readCover stb name clist
     ; return (stb, stmt : stmts)
     }

takeTLStmt (stb, stmts) (P.Cross names) =
  do { stmt <- readCross stb names
     ; return (stb, stmt : stmts)
     }

data Script = Script { scrMods :: SymbolTable Module
                     , scrStmts :: [TLStmt]
                     }

run :: [P.TLStmt] -> ErrorsOr Script
run pstmts = do { (stb, stmts) <- foldEO takeTLStmt (stbEmpty, []) pstmts
                ; return $ Script (stbToSymbolTable stb) (reverse stmts)
                }
