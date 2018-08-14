module Grouping
  ( run
  , Record(..)
  , BitsRecord(..)
  , Group(..)
  , Module(..)
  ) where

import Data.Functor

import qualified Parser as P

import Ranged
import ErrorsOr

{-
  This pass is in charge of putting when/group blocks into a canonical
  form. A when block that contains multiple groups is split, so the
  end result is a list of groups, each of which may have a guard and
  contains one or more record statements.
-}
data Record = Record { recExpr :: Ranged P.Expression
                     , recSym :: Maybe (Ranged P.Symbol)
                     , recCov :: Maybe [(Ranged Integer, Ranged Integer)]
                     }

data BitsRecord = BitsRecord { brExpr :: Ranged P.Expression
                             , brSym :: Maybe (Ranged P.Symbol)
                             }

{-
  A group either contains a list of records which form an implicit
  cross or it contains a single record as a "BitsRecord". This means
  we want to see each bit in the recorded expression as 1 and 0.
-}
data Group = Group { grpGuards :: [Ranged P.Expression]
                   , grpRecords :: Either [Record] BitsRecord
                   , grpScopes :: Maybe String
                   }

data Module = Module (Ranged P.Symbol) [Ranged P.Port] [Group]

-- Wrap a record statement in a group containing just itself
wrapRecord :: [Ranged P.Expression] -> P.RecordStmt -> Group
wrapRecord guards stmt = Group guards record (P.recScopes stmt)
  where recExpr = P.recExpr stmt
        recSym = P.recSym stmt
        record = case P.recCov stmt of
                   Nothing ->
                     Left $ [Record recExpr recSym Nothing]
                   Just (P.CoverList vals) ->
                     Left $ [Record recExpr recSym (Just vals)]
                   Just P.CoverBits ->
                     Right $ BitsRecord recExpr recSym

-- Read a statement, either at the top level or inside some when {}
-- block. The first argument is the list of guards that we've seen so
-- far.
readStmt :: [Ranged P.Expression] -> Ranged P.Statement -> ErrorsOr [Group]
readStmt guards rstmt = f (rangedData rstmt)
  where rng = rangedRange rstmt
        -- A top-level record statement is always allowed. We need to
        -- wrap it up in a group.
        f (P.Record recstmt) = good $ [wrapRecord guards recstmt]
        -- For a when statement, we need to add the guard to guards
        -- and recurse.
        f (P.When (P.WhenStmt guard stmts)) =
          concat <$> mapEO (readStmt (guard : guards)) stmts
        -- For a group statement, we use groupReadStmt to read all the
        -- statements inside it (which had better be Record statements
        -- with no scopes), then wrap them up in a single group.
        f (P.Group (P.GroupStmt stmts scopes)) =
          do { body <- mapEO groupReadStmt stmts
             ; good [Group guards (Left body) scopes]
             }

-- Read a statement inside a group block. This had better be a Record
-- statement, and may not have a match_scopes part.
groupReadStmt :: Ranged P.Statement -> ErrorsOr Record
groupReadStmt rstmt = f (rangedData rstmt)
  where rng = rangedRange rstmt
        erk x = bad1 $ Ranged rng x
        f (P.Record recStmt) = g recStmt
        f (P.When _) = erk "when block nested inside group."
        f (P.Group _) = erk "nested groups."
        g (P.RecordStmt _ _ _ (Just scopes)) =
          erk "record with `match_scopes' inside group."
        g (P.RecordStmt expr as (Just P.CoverBits) Nothing) =
          erk "record with `cover bits' inside group."
        g (P.RecordStmt expr as (Just (P.CoverList vals)) Nothing) =
          good $ Record expr as (Just vals)
        g (P.RecordStmt expr as Nothing Nothing) =
          good $ Record expr as Nothing

readModule :: P.Module -> ErrorsOr Module
readModule (P.Module name ports stmts) =
  (Module name ports . concat) <$> mapEO (readStmt []) stmts

run :: [P.Module] -> ErrorsOr [Module]
run = mapEO readModule
