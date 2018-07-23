module Merge
  ( Coverage(..)
  , ModCoverage(..)
  , ScopeCoverage(..)
  , GroupCoverage(..)
  , mergeCoverage
  ) where

import Ranged
import SymbolTable

import qualified Raw

import qualified Parser as P
import qualified Expressions as E
import qualified Width as W

import Control.Monad

import Data.Array
import Data.Bits
import qualified Data.Foldable as Foldable
import Data.Functor ((<$>))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Numeric (showHex)

{-
  This module is in charge of merging a raw coverage report with a
  parsed coverage script. This might fail.
-}

data Coverage = Coverage { covTests :: Int
                         , covMods :: [ModCoverage]
                         }

modName :: W.Module -> String
modName = P.symName . rangedData . W.modName

mergeCoverage :: [W.Module] -> Raw.Coverage -> Either String Coverage
mergeCoverage mods raw = Coverage (Raw.covCount raw) <$> mapM f mods
  where f mod = mergeMod mod (Raw.getModData (modName mod) raw)

data ModCoverage = ModCoverage String [ScopeCoverage]

mergeMod :: W.Module -> Raw.ModData -> Either String ModCoverage
mergeMod mod md =
  (ModCoverage (modName mod) . Map.elems) <$>
  Raw.traverseMD (mergeScope mod) md

data ScopeCoverage = ScopeCoverage String [GroupCoverage]

mergeScope :: W.Module -> String -> Raw.ScopeData ->
              Either String ScopeCoverage
mergeScope mod scope sd =
  if Raw.sdMaxKey sd >= length (W.modGroups mod) then
    Left $
    "Maximum group key for module at scope " ++ scope ++
    " is " ++ show (Raw.sdMaxKey sd) ++
    ", which overflows the expected group length."
  else
    ScopeCoverage scope <$>
    (mapM mergeGrp $ zip (W.modGroups mod) (map (Raw.sdGetGroup sd) [0..]))

data GroupCoverage = GroupCoverage { gcVals :: Set.Set Integer
                                   , gcST :: SymbolTable ()
                                   , gcRecs :: [W.Record]
                                   }

checkWidth :: Int -> Integer -> Either String ()
checkWidth w n =
  if shiftR n w /= 0 then
    Left ("The value " ++ (showHex n "") ++ " is more than " ++
          show w ++ " bits wide. Are your acov.log files out of date?")
  else
    Right ()

checkWidths :: [W.Record] -> Set.Set Integer -> Either String ()
checkWidths recs vals = Foldable.traverse_ (checkWidth w) vals
  where w = sum (map W.recWidth recs)

mergeGrp :: (W.Group, Set.Set Integer) -> Either String GroupCoverage
mergeGrp (grp, vals) =
  do { checkWidths recs vals
     ; return $ GroupCoverage vals (W.grpST grp) recs
     }
  where recs = W.grpRecs grp
