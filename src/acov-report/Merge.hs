module Merge
  ( Coverage(..)
  , ModCoverage(..)
  , ScopeCoverage(..)
  , GroupCoverage(..)
  , RecsCoverage(..)
  , BRecCoverage(..)
  , mergeCoverage
  ) where

import Ranged

import qualified Raw

import qualified Parser as P
import qualified Expressions as E
import qualified Width as W

import Control.Applicative
import Control.Monad

import Data.Array
import Data.Bits
import qualified Data.Foldable as Foldable
import Data.Functor ((<$>))
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
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
mergeCoverage mods raw =
  Coverage (Raw.covCount raw) <$> mapM f (zip [0..] mods)
  where f (modIdx, mod) = mergeMod mod (Raw.getModData modIdx raw)

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
    ScopeCoverage scope <$> mapM (mergeGrp sd) (zip [0..] (W.modGroups mod))

data RecsCoverage = RecsCoverage [W.Record] (Set.Set Integer)
data BRecCoverage = BRecCoverage W.BitsRecord (Set.Set Int, Set.Set Int)

newtype GroupCoverage = GroupCoverage (Either RecsCoverage BRecCoverage)

checkWidth :: Int -> Integer -> Either String ()
checkWidth w n =
  if shiftR n w /= 0 then
    Left ("The value " ++ (showHex n "") ++ " is more than " ++
          show w ++ " bits wide. Are your acov.log files out of date?")
  else
    Right ()

checkWidths :: Int -> Set.Set Integer -> Either String ()
checkWidths w = Foldable.traverse_ (checkWidth w)

takeBitIdx :: Int -> Integer -> Either String (Maybe Int)
takeBitIdx width bit =
  if bit < 0 then
    Left $ "The index " ++ show bit ++ " is negative, which is invalid."
  -- We have to round up a bit before throwing an error here because
  -- the C++ code doesn't know the width of the record and will add
  -- some extra zero bits seen.
  else if bit >= 64 + toInteger width then
    Left $ ("The index " ++ show bit ++ " is too big for the width (" ++
            show width ++ ").")
  -- If the bit is not valid, hopefully this was a zero that the C++
  -- code added spuriously. Ignore it.
  else if bit >= toInteger width then
    Right $ Nothing
  else
    Right $ Just $ fromInteger bit

takeBitIndices :: Int -> Set.Set Integer -> Set.Set Integer ->
                  Either String (Set.Set Int, Set.Set Int)
takeBitIndices w ones zeros = liftA2 (,) (get ones) (get zeros)
  where get vals = do { ints' <- mapM (takeBitIdx w) (Set.toAscList vals)
                      ; return $ Set.fromAscList $ catMaybes ints'
                      }

mergeGrp :: Raw.ScopeData -> (Int, W.Group) -> Either String GroupCoverage
mergeGrp sd (idx, grp) =
  case W.grpRecs grp of
    Left recs -> checkWidths width vals >>
                 (return $ GroupCoverage $ Left $ RecsCoverage recs vals)
    Right brec -> (GroupCoverage . Right . BRecCoverage brec) <$>
                  (takeBitIndices width ones zeros)
  where vals = Raw.sdGetGroup sd idx
        ones = vals
        zeros = Raw.sdGetGroup sd (- (idx + 1))
        width = W.grpWidth grp
