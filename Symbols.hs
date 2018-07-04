module Symbols
  ( run
  , Symbol(..)
  , Expression(..)
  , Atom(..)
  , ModSymbolTable(..)
  , ModSymIdx(..)
  , SymbolArray(..)
  , SymbolEntry(..)
  , mapSE , mapSE'
  , TLStmt(..)
  , ModStmt(..)
  , Module(..)
  , Script(..)
  , symbolData
  ) where

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

import qualified Parser as P
import qualified When as W
import ErrorsOr
import Ranged

newtype Symbol = Symbol Int
  deriving Show

data DottedSymbol = DottedSymbol Symbol Symbol
  deriving Show

data Atom = AtomSym Symbol
          | AtomInt P.VInt
  deriving Show

data Expression = ExprAtom Atom
                | ExprParens (Ranged Expression)
                | ExprSel (Ranged Expression)
                  (Ranged Expression) (Maybe (Ranged Expression))
                | ExprConcat [Ranged Expression]
                | ExprReplicate (Ranged Expression) (Ranged Expression)
                | ExprUnOp (Ranged P.UnOp) (Ranged Expression)
                | ExprBinOp (Ranged P.BinOp)
                  (Ranged Expression) (Ranged Expression)
                | ExprCond (Ranged Expression)
                  (Ranged Expression) (Ranged Expression)
  deriving Show

data ModStmt = Assign (Ranged Symbol) (Ranged Expression)
             | Record (Maybe (Ranged Symbol))
               (Ranged Expression) (Ranged Symbol)
  deriving Show

data Module = Module ModSymbolTable [ModStmt]
  deriving Show

data TLStmt = Cover (Ranged DottedSymbol) (Maybe P.CoverList)
            | Cross [Ranged DottedSymbol]
  deriving Show

{-
  A module will be represented by a symbol table, together with a list
  of statements.

  First, we define how to build a symbol table for a module. The end
  result will be a ModSymbolTable. The first parameter is a map from
  name (wrapped up as a P.Symbol) to a generalised index (a type and
  an integer).

  The other parameters give ports, triggers and records.
-}
data IType = PortType | TriggerType
  deriving Show

data ModSymIdx = ModSymIdx IType Symbol
  deriving Show

data SymbolEntry a = SymbolEntry P.Symbol a
  deriving Show

symbolEntryData :: SymbolEntry a -> a
symbolEntryData (SymbolEntry sym a) = a

mapSE :: Functor f => (a -> f b) -> SymbolEntry a -> f (SymbolEntry b)
mapSE f (SymbolEntry sym a) = SymbolEntry sym <$> f a

mapSE' :: Functor f =>
          (P.Symbol -> a -> f b) -> SymbolEntry a -> f (SymbolEntry b)
mapSE' f (SymbolEntry sym a) = SymbolEntry sym <$> f sym a

newtype SymbolArray a = SymbolArray (Array Int (SymbolEntry a))
  deriving Show

symbolData :: SymbolArray a -> Symbol -> a
symbolData (SymbolArray a) (Symbol i) = symbolEntryData (a ! i)

data ModSymbolTable = ModSymbolTable { mstMap :: Map.Map String ModSymIdx
                                     , mstPorts :: SymbolArray (Ranged (Maybe P.Slice))
                                     , mstTriggers :: SymbolArray ()
                                     , mstRMap :: Map.Map String Symbol
                                     , mstRecords :: SymbolArray ()
                                     }
  deriving Show

{-
  As well as a module symbol table for each module, we have a symbol
  table for the entire file (that remembers module names). Wrapping
  that up with the lists of modules and top-level statements gives the
  entire script.
-}
data Script = Script { scrMap :: Map.Map String Symbol
                     , scrModules :: SymbolArray Module
                     , scrStmts :: [TLStmt]
                     }
  deriving Show

{-
  To build a ModSymbolTable, we use an MSBuilder.

  We work through the module's statements in order with a foldl' but
  cons elements on to the list, so msbPorts, msbTriggers, msbRecords
  and msbStatements all come out backwards.
-}
data CList a = CList Int [a]

clCons :: a -> CList a -> CList a
clCons a (CList n as) = CList (n + 1) (a : as)

clLen :: CList a -> Int
clLen (CList n _) = n

clEmpty :: CList a
clEmpty = CList 0 []

clArray :: CList a -> Array Int a
clArray (CList n as) = listArray (0, n - 1) (reverse as)

clMap :: (a -> b) -> CList a -> CList b
clMap f (CList n as) = CList n $ map f as

clAt :: Symbol -> CList a -> a
clAt (Symbol idx) (CList n as) = as !! (n - 1 - idx)

data MSBuilder =
  MSBuilder { msbMap :: Map.Map String ModSymIdx
            , msbPorts :: CList (SymbolEntry (Ranged (Maybe P.Slice)))
            , msbTriggers :: CList P.Symbol
            , msbRMap :: Map.Map String Symbol
            , msbRecords :: CList P.Symbol
            , msbStatements :: [ModStmt]
            }

msbInit :: MSBuilder
msbInit = MSBuilder Map.empty clEmpty clEmpty Map.empty clEmpty []

{-
  Given an MSBuilder, we can intern expressions. Doing it with the
  builder rather than the eventual symbol table means we can spot
  errors like this:

    module a (foo [10:0]) {
      x = foo [0];
      trigger x;
    }
-}

msbAtom :: MSBuilder -> LCRange -> P.Atom -> ErrorsOr Expression
msbAtom msb _ (P.AtomInt vint) = good $ ExprAtom (AtomInt vint)
msbAtom msb rng (P.AtomSym psym) =
  case Map.lookup (P.symName psym) (msbMap msb) of
    Nothing -> bad1 $ (Ranged rng $
                       "undeclared symbol: " ++ P.symName psym ++ ".")
    Just (ModSymIdx t sym) ->
      case t of
        PortType -> good $ ExprAtom (AtomSym sym)
        TriggerType -> bad1 (Ranged rng $
                             "symbol `" ++ P.symName psym ++
                             "' names a trigger, not a port.")

msbParens :: MSBuilder -> Ranged P.Expression -> ErrorsOr Expression
msbParens msb expr = ExprParens <$> msbExpression msb expr

msbSel :: MSBuilder -> Ranged P.Expression ->
          Ranged P.Expression -> Maybe (Ranged P.Expression) ->
          ErrorsOr Expression
msbSel msb var top bot =
  liftA3 ExprSel (intern var) (intern top) (eoMaybe (intern <$> bot))
  where intern = msbExpression msb

msbConcat :: MSBuilder -> [Ranged P.Expression] -> ErrorsOr Expression
msbConcat msb exprs = ExprConcat <$> mapEO (msbExpression msb) exprs

msbReplicate :: MSBuilder -> Ranged P.Expression ->
                Ranged P.Expression -> ErrorsOr Expression
msbReplicate msb count val = liftA2 ExprReplicate (intern count) (intern val)
  where intern = msbExpression msb

msbUnOp :: MSBuilder -> Ranged P.UnOp -> Ranged P.Expression ->
           ErrorsOr Expression
msbUnOp msb unop expr = ExprUnOp unop <$> msbExpression msb expr

msbBinOp :: MSBuilder -> Ranged P.BinOp -> Ranged P.Expression ->
            Ranged P.Expression -> ErrorsOr Expression
msbBinOp msb binop e0 e1 = liftA2 (ExprBinOp binop) (intern e0) (intern e1)
  where intern = msbExpression msb

msbCond :: MSBuilder -> Ranged P.Expression -> Ranged P.Expression ->
           Ranged P.Expression -> ErrorsOr Expression
msbCond msb e0 e1 e2 = liftA3 ExprCond (intern e0) (intern e1) (intern e2)
  where intern = msbExpression msb

msbExpression' :: MSBuilder -> LCRange -> P.Expression -> ErrorsOr Expression
msbExpression' msb rng (P.ExprAtom a) = msbAtom msb rng a
msbExpression' msb _ (P.ExprParens e) = msbParens msb e
msbExpression' msb _ (P.ExprSel v t b) = msbSel msb v t b
msbExpression' msb _ (P.ExprConcat es) = msbConcat msb es
msbExpression' msb _ (P.ExprReplicate c v) = msbReplicate msb c v
msbExpression' msb _ (P.ExprUnOp uo e) = msbUnOp msb uo e
msbExpression' msb _ (P.ExprBinOp bo e0 e1) = msbBinOp msb bo e0 e1
msbExpression' msb _ (P.ExprCond e0 e1 e2) = msbCond msb e0 e1 e2

msbExpression :: MSBuilder -> Ranged P.Expression -> ErrorsOr (Ranged Expression)
msbExpression msb rexpr =
  copyRange rexpr <$> msbExpression' msb (rangedRange rexpr) (rangedData rexpr)

{-
  Now we define the various methods that update the MSBuilder state
  as it reads statements.
-}

addSym :: P.Symbol -> IType -> Int -> MSBuilder -> MSBuilder
addSym psym typ n msb =
  msb { msbMap = Map.insert (P.symName psym)
                 (ModSymIdx typ (Symbol n)) (msbMap msb) }

msbTakePort :: MSBuilder -> Ranged P.Port -> ErrorsOr MSBuilder
msbTakePort msb rp =
  if Map.member (P.symName psym) (msbMap msb)
  then bad1 (copyRange rp $
              "duplicate port in module: " ++ P.symName psym ++ ".")
  else good (addSym psym PortType nports msb
              { msbPorts =
                clCons (SymbolEntry psym (copyRange rp slice)) ports })
  where P.Port psym slice = rangedData rp
        ports = msbPorts msb
        nports = clLen ports

msbTakePorts :: MSBuilder -> [Ranged P.Port] -> ErrorsOr MSBuilder
msbTakePorts = foldEO msbTakePort

msbTakeTrigger :: MSBuilder -> Ranged P.Symbol -> ErrorsOr MSBuilder
msbTakeTrigger msb rpsym =
  if Map.member (P.symName psym) (msbMap msb)
  then bad1 (copyRange rpsym
              "trigger declaration shadows existing symbol.")
  else good (addSym psym TriggerType ntrigs msb
              { msbTriggers = clCons psym trigs })
  where psym = rangedData rpsym
        trigs = msbTriggers msb
        ntrigs = clLen trigs

msbTrigger :: MSBuilder -> Ranged P.Symbol -> ErrorsOr (Ranged Symbol)
msbTrigger msb rpsym =
  case Map.lookup name (msbMap msb) of
    Nothing -> bad1 $ (copyRange rpsym $
                       "assigning to undeclared symbol `" ++ name ++ "'.")
    Just (ModSymIdx t sym) ->
      case t of
        PortType -> bad1 $ (copyRange rpsym $
                            "assigning to port symbol `" ++ name ++ "'.")
        TriggerType -> good $ copyRange rpsym sym
  where name = P.symName (rangedData rpsym)

msbAddStmt :: ModStmt -> MSBuilder -> MSBuilder
msbAddStmt stmt msb = msb { msbStatements = stmt : (msbStatements msb) }

msbTakeAssign :: MSBuilder -> Ranged P.Symbol ->
                 Ranged P.Expression -> ErrorsOr MSBuilder
msbTakeAssign msb rpsym expr =
  liftA2 addAssign (msbTrigger msb rpsym) (msbExpression msb expr)
  where addAssign rsym rexpr = msbAddStmt (Assign rsym rexpr) msb

msbTakeRecName :: MSBuilder -> Ranged P.Symbol ->
                  ErrorsOr (Ranged Symbol, MSBuilder)
msbTakeRecName msb rpsym =
  if Map.member name (msbRMap msb)
  then bad1 (copyRange rpsym $
             "duplicate records with name `" ++ name ++ "' in module.")
  else good $ (copyRange rpsym sym,
               msb { msbRMap = Map.insert name sym (msbRMap msb)
                   , msbRecords = clCons psym (msbRecords msb)
                   })
  where psym = rangedData rpsym
        name = P.symName psym
        sym = Symbol (clLen (msbRecords msb))

msbTakeRecord :: MSBuilder -> Maybe (Ranged P.Symbol) ->
                 Ranged P.Expression -> Ranged P.Symbol ->
                 ErrorsOr MSBuilder
msbTakeRecord msb trig expr recname =
  liftA3 takeIt
  (eoMaybe (msbTrigger msb <$> trig))
  (msbExpression msb expr)
  (msbTakeRecName msb recname)
  where takeIt sym' expr' (recname', msb') =
          msbAddStmt (Record sym' expr' recname') msb'

msbTakeStmt :: MSBuilder -> W.ModStmt -> ErrorsOr MSBuilder
msbTakeStmt msb (W.Trigger psym) = msbTakeTrigger msb psym
msbTakeStmt msb (W.Assign lhs rhs) = msbTakeAssign msb lhs rhs
msbTakeStmt msb (W.Record trig expr name) = msbTakeRecord msb trig expr name

msbTakeStmts :: MSBuilder -> [W.ModStmt] -> ErrorsOr MSBuilder
msbTakeStmts = foldEO msbTakeStmt

msbBuild :: [Ranged P.Port] -> [W.ModStmt] -> ErrorsOr MSBuilder
msbBuild ports stmts = do { msb <- msbTakePorts msbInit ports
                          ; msbTakeStmts msb stmts
                          }

{-
  Finally, we need to define how to convert an MSBuilder to an actual
  symbol table.
-}
makeSymbolArray :: CList P.Symbol -> SymbolArray ()
makeSymbolArray cl = SymbolArray (clArray (clMap (\ s -> SymbolEntry s ()) cl))

msbToModule :: MSBuilder -> Module
msbToModule msb =
  Module (ModSymbolTable (msbMap msb)
           (SymbolArray $ clArray (msbPorts msb))
           (makeSymbolArray $ msbTriggers msb)
           (msbRMap msb)
           (makeSymbolArray $ msbRecords msb))
         (msbStatements msb)

moduleContents :: [Ranged P.Port] -> [W.ModStmt] -> ErrorsOr Module
moduleContents ports stmts = msbToModule <$> msbBuild ports stmts

{-
  Now we define a TLBuilder, used for building the top-level symbol
  table that becomes a Script object.
-}
data TLBuilder = TLBuilder { tlbMap :: Map.Map String Symbol
                           , tlbModules :: CList (SymbolEntry Module)
                           , tlbStmts :: [TLStmt]
                           }


tlbInit :: TLBuilder
tlbInit = TLBuilder Map.empty clEmpty []

tlbRegModule :: TLBuilder -> Ranged P.Symbol -> Module -> ErrorsOr TLBuilder
tlbRegModule tlb rpname mod =
  if Map.member name (tlbMap tlb)
  then bad1 (copyRange rpname $
             "duplicate module with name `" ++ name ++ "'.")
  else good (tlb { tlbMap = Map.insert name sym (tlbMap tlb)
                 , tlbModules = clCons symEntry (tlbModules tlb) })
  where pname = rangedData rpname
        name = P.symName pname
        sym = Symbol (clLen (tlbModules tlb))
        symEntry = SymbolEntry pname mod

tlbTakeModule :: TLBuilder -> Ranged P.Symbol -> 
                 [Ranged P.Port] -> [W.ModStmt] -> ErrorsOr TLBuilder
tlbTakeModule tlb rpname ports stmts =
  moduleContents ports stmts >>= tlbRegModule tlb rpname

tlbGetModule :: TLBuilder -> LCRange -> P.Symbol -> ErrorsOr (Symbol, Module)
tlbGetModule tlb rng psym =
  case Map.lookup (P.symName psym) (tlbMap tlb) of
    Nothing -> bad1 (Ranged rng $
                     "no such module: `" ++ P.symName psym ++ "'.")
    Just sym -> good (sym, symbolEntryData (clAt sym (tlbModules tlb)))

modGetSymbol :: Module -> LCRange -> P.Symbol -> ErrorsOr Symbol
modGetSymbol (Module mst _) rng psym =
  case Map.lookup (P.symName psym) (mstRMap mst) of
    Nothing -> bad1 (Ranged rng $
                     "module does not define a record with name `" ++ 
                     P.symName psym ++ "'.")
    Just sym -> good sym
 
tlbDottedSymbol :: TLBuilder -> Ranged P.DottedSymbol ->
                   ErrorsOr (Ranged DottedSymbol)
tlbDottedSymbol tlb rds =
  do { (sym, mod) <- tlbGetModule tlb (rangedRange rds) mname
     ; record <- modGetSymbol mod (rangedRange rds) sname
     ; return $ copyRange rds (DottedSymbol sym record)
     }
  where P.DottedSymbol mname sname = rangedData rds

tlbAddStmt :: TLBuilder -> TLStmt -> TLBuilder
tlbAddStmt tlb stmt = tlb { tlbStmts = stmt : (tlbStmts tlb) }

tlbTakeCover :: TLBuilder -> Ranged P.DottedSymbol -> Maybe P.CoverList ->
                ErrorsOr TLBuilder
tlbTakeCover tlb rpds coverlist =
  do { rds <- tlbDottedSymbol tlb rpds
     ; return $ tlbAddStmt tlb (Cover rds coverlist)
     }

tlbTakeCross :: TLBuilder -> [Ranged P.DottedSymbol] -> ErrorsOr TLBuilder
tlbTakeCross tlb lst =
  (tlbAddStmt tlb . Cross) <$> mapEO (tlbDottedSymbol tlb) lst

tlbTakeStmt :: TLBuilder -> W.TLStmt -> ErrorsOr TLBuilder
tlbTakeStmt tlb (W.Module name pts stmts) = tlbTakeModule tlb name pts stmts
tlbTakeStmt tlb (W.Cover name clist) = tlbTakeCover tlb name clist
tlbTakeStmt tlb (W.Cross names) = tlbTakeCross tlb names

tlbTakeStmts :: TLBuilder -> [W.TLStmt] -> ErrorsOr TLBuilder
tlbTakeStmts = foldEO tlbTakeStmt

tlbReadScript :: W.Script -> ErrorsOr TLBuilder
tlbReadScript (W.Script stmts) = tlbTakeStmts tlbInit stmts

tlbToScript :: TLBuilder -> Script
tlbToScript tlb = Script
                  (tlbMap tlb)
                  (SymbolArray $ clArray (tlbModules tlb))
                  (tlbStmts tlb)

run :: W.Script -> ErrorsOr Script
run = fmap tlbToScript . tlbReadScript
