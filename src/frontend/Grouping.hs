module Grouping
  ( run
  , Record(..)
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
data Record = Record (Ranged P.Expression)
              (Maybe (Ranged P.Symbol)) (Maybe P.CoverList)

data Group = Group (Maybe (Ranged P.Expression)) [Record]

data Module = Module (Ranged P.Symbol) [Ranged P.Port] [Group]

tlReadStmt :: Ranged P.Statement -> ErrorsOr [Group]
tlReadStmt rstmt = f (rangedData rstmt)
  where rng = rangedRange rstmt
        erk = bad1 . Ranged rng
        f (P.Record expr as clist) = good [Group Nothing [Record expr as clist]]
        f (P.When guard stmts) = mapEO (whenReadStmt (Just guard)) stmts
        f (P.Group stmts) = do { body <- mapEO groupReadStmt stmts
                               ; good [Group Nothing body]
                               }

whenReadStmt :: Maybe (Ranged P.Expression) -> Ranged P.Statement ->
                ErrorsOr Group
whenReadStmt guard rstmt = Group guard <$> f (rangedData rstmt)
  where rng = rangedRange rstmt
        erk x = bad1 $ Ranged rng x
        f (P.Record expr as clist) = good $ [Record expr as clist]
        f (P.When _ _) = erk "Nested when blocks."
        f (P.Group stmts) = mapEO groupReadStmt stmts

groupReadStmt :: Ranged P.Statement -> ErrorsOr Record
groupReadStmt rstmt = f (rangedData rstmt)
  where rng = rangedRange rstmt
        erk x = bad1 $ Ranged rng x
        f (P.Record expr as clist) = good $ Record expr as clist
        f (P.When _ _) = erk "when block nested inside group."
        f (P.Group _) = erk "nested groups."

readModule :: P.Module -> ErrorsOr Module
readModule (P.Module name ports stmts) =
  (Module name ports . concat) <$> mapEO tlReadStmt stmts

run :: [P.Module] -> ErrorsOr [Module]
run = mapEO readModule
