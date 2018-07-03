module When where

{-
  The when pass is the first pass after the parser. This simplifies
  some data structures (flattening out when blocks) to simplify the
  work of downstream passes. Expressions are left completely
  unchanged.
-}

import Data.Functor ((<$>))
import qualified Parser as P
import ErrorsOr
import Ranged

-- "Record (Just x) expr (Just y)" comes from "when x record expr as y".
data ModStmt = Trigger (Ranged P.Symbol)
             | Assign (Ranged P.Symbol) (Ranged P.Expression)
             | Record (Maybe (Ranged P.Symbol))
               (Ranged P.Expression) (Ranged P.Symbol)
  deriving Show

data TLStmt = Module (Ranged P.Symbol) [Ranged P.Port] [ModStmt]
            | Cover (Ranged P.DottedSymbol) (Maybe P.CoverList)
            | Cross [Ranged P.DottedSymbol]
  deriving Show

newtype Script = Script [TLStmt]
  deriving Show

{-
  readRecord is called to make sense of a record statement. This might
  be inside a when block. We check that either there is an explicit
  record name or the thing being recorded is a symbol (in which case
  the record name defaults to the symbol name).
-}
readRecord :: Maybe (Ranged P.Symbol) ->
              Ranged P.Expression -> Maybe (Ranged P.Symbol) ->
              ErrorsOr ModStmt
readRecord when rexpr (Just rname) = good $ Record when rexpr rname
readRecord when rexpr Nothing =
  case rangedData rexpr of
    P.ExprAtom (P.AtomSym sym) ->
      good $ Record when rexpr (copyRange rexpr sym)
    _ -> bad1 (copyRange rexpr
               "record for non-symbol expression with no explicit name")

readWhen' :: Ranged P.Symbol -> Ranged P.Stmt -> ErrorsOr ModStmt
readWhen' rguard (Ranged _ (P.StmtRecord re rsym)) =
  readRecord (Just rguard) re rsym
readWhen' _ rstmt =
  bad1 (copyRange rstmt "non-record statement in a when block")

readWhen :: Ranged P.Symbol -> [Ranged P.Stmt] -> ErrorsOr [ModStmt]
readWhen (Ranged rng (P.Symbol sym)) [] =
  bad1 (Ranged rng $ "when block has no guarded record statements.")
readWhen rsym as = mapEO (readWhen' rsym) as

readStmt :: P.Stmt -> ErrorsOr [ModStmt]
readStmt (P.StmtTrigger sym) = good [Trigger sym]
readStmt (P.StmtAssign sym re) = good [Assign sym re]
readStmt (P.StmtRecord re sym) = (\ x -> [x]) <$> readRecord Nothing re sym
readStmt (P.StmtWhen sym stmts) = readWhen sym stmts

readStmts :: [P.Stmt] -> ErrorsOr [ModStmt]
readStmts = fmap concat . mapEO readStmt

readTLStmt :: P.TLStmt -> ErrorsOr TLStmt
readTLStmt (P.Module name ports stmts) = Module name ports <$> readStmts stmts
readTLStmt (P.Cover name clist) = good (Cover name clist)
readTLStmt (P.Cross names) = good (Cross names)

run :: [P.TLStmt] -> ErrorsOr Script
run = fmap Script . mapEO readTLStmt
