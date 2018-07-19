module Records
  ( Group(..),
    Module(..),
    run
  ) where

import Control.Exception.Base
import Data.Functor
import qualified Data.Map.Strict as Map

import qualified Parser as P
import qualified Expressions as E
import qualified Width as W

import SymbolTable
import Ranged
import ErrorsOr

{-
  This module simplifies the Group objects by coalescing the record
  statements. Instead of a list of statements that look like they run
  in sequence (which wouldn't be atomic, so wouldn't actually work),
  we end up grouping by type.

  We also check that every record is used exactly once in a cover
  (which also implies that any cross is with variables that are also
  hit by a unique cover statement).
-}
type Record = (Ranged E.Expression, Maybe P.CoverList)
type Cross = [Ranged Symbol]

data Group = Group { grpST :: SymbolTable Int
                   , grpGuard :: Maybe (Ranged E.Expression)
                   , grpRecs :: Map.Map Symbol Record
                   , grpCross :: [Cross]
                   }

data Module = Module { modName :: Ranged P.Symbol
                     , modSyms :: SymbolTable (Ranged E.Slice)
                     , modGroups :: [Group]
                     }

{-
  To do this reshuffling, we iterate over the statements in a W.Group.
  Record statements get placed in a map (indexed by the symbol that is
  recorded). Cross statements just get placed in a list (they are the
  easy ones). Cover statements also get placed in a map.

  If all goes to plan, both maps will be the same length at the end
  and we can just zip across their elements to generate the Record
  objects.
-}
type GrpBuilder = (Map.Map Symbol (Ranged E.Expression),
                   Map.Map Symbol (Maybe P.CoverList),
                   [Cross])

takeStmt :: GrpBuilder -> E.Statement -> ErrorsOr GrpBuilder

takeStmt (rmap, cmap, xs) (E.Record expr rsym) =
  assert (not $ Map.member sym rmap) good (Map.insert sym expr rmap, cmap, xs)
  where sym = rangedData rsym

takeStmt (rmap, cmap, xs) (E.Cover rsym clist) =
  if Map.member sym cmap then
    bad1 $ copyRange rsym "Duplicate cover statement for symbol."
  else
    good $ (rmap, Map.insert sym clist cmap, xs)
  where sym = rangedData rsym

takeStmt (rmap, cmap, xs) (E.Cross syms) = good $ (rmap, cmap, syms : xs)

readGroup :: W.Group -> ErrorsOr Group
readGroup (W.Group st guard stmts) =
  do { (rmap, cmap, xs) <- foldEO takeStmt (Map.empty, Map.empty, []) stmts
     ; if Map.size rmap /= Map.size cmap then
         let badSyms = Map.difference rmap cmap in
           assert (not $ Map.null badSyms) $
           bad $ map (unusedRec st . fst) (Map.toAscList badSyms)
       else
         good ()
     ; let recs = Map.fromAscList
                  (map combine (zip (Map.toAscList rmap) (Map.toAscList cmap)))
     ; good $ Group st guard recs (reverse xs)
     }
  where combine ((rsym, expr), (csym, clist)) =
          assert (rsym == csym) (rsym, (expr, clist))

unusedRec :: SymbolTable Int -> Symbol -> Ranged String
unusedRec st sym = copyRange (stNameAt sym st)
                   "Unused record (not hit by any cover statement)."

readModule :: W.Module -> ErrorsOr Module
readModule mod = Module (W.modName mod) (W.modSyms mod) <$>
                 mapEO readGroup (W.modGroups mod)

run :: [W.Module] -> ErrorsOr [Module]
run = mapEO readModule
