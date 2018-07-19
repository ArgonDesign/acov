module SymbolTable
  ( Symbol
  , SymbolTable
  , stLookup
  , stAt
  , stNameAt
  , stGet
  , stAssocs
  , stTraverseWithSym
  , stTraverseWithSym_
  , stMapWithSymbol
  , STBuilder
  , stbEmpty
  , stbAdd
  , stbLastSymbol
  , stbGet
  , stbToSymbolTable
  ) where

import Control.Applicative
import qualified Data.Map.Strict as Map
import Data.Array.IArray
import qualified Data.Foldable as Foldable
import Data.Functor
import Data.Traversable

import CList
import ErrorsOr
import Ranged

import qualified Parser as P

newtype Symbol = Symbol Int
  deriving (Eq, Ord)

mapGet :: String -> LCRange -> P.Symbol -> Map.Map String Int -> ErrorsOr Int
mapGet what rng psym map =
  case Map.lookup name map of
    Just idx -> good $ idx
    Nothing -> bad1 (Ranged rng $
                     "no such " ++ what ++ ": `" ++ name ++ "'.")
  where name = P.symName psym


data SymbolTable a =
  SymbolTable { stMap :: Map.Map String Int
              , stData :: Array Int (Ranged P.Symbol, a)
              }

instance Functor SymbolTable where
  fmap f st = SymbolTable (stMap st) (f' <$> (stData st))
    where f' (sym, a) = (sym, f a)

instance Foldable.Foldable SymbolTable where
  foldMap f st = Foldable.foldMap (f . snd) (stData st)
  foldr f b st = Foldable.foldr f' b (stData st)
    where f' (_, a) b = f a b

instance Traversable SymbolTable where
  traverse f st = SymbolTable (stMap st) <$> (traverse f' (stData st))
    where f' (sym, a) = (,) sym <$> f a

stLookup :: P.Symbol -> SymbolTable a -> Maybe Symbol
stLookup sym st = Symbol <$> Map.lookup (P.symName sym) (stMap st)

stAt :: Symbol -> SymbolTable a -> a
stAt (Symbol idx) st = snd $ (stData st) ! idx

stNameAt :: Symbol -> SymbolTable a -> Ranged P.Symbol
stNameAt (Symbol idx) st = fst $ (stData st) ! idx

stGet :: String -> LCRange -> P.Symbol -> SymbolTable a -> ErrorsOr (Symbol, a)
stGet what rng psym st =
  do { idx <- mapGet what rng psym (stMap st)
     ; good $ (Symbol idx, snd $ (stData st) ! idx)
     }

stAssocs :: SymbolTable a -> [(Ranged P.Symbol, a)]
stAssocs = elems . stData

stTraverseWithSym :: Applicative t =>
                     (Ranged P.Symbol -> a -> t b) ->
                     SymbolTable a -> t (SymbolTable b)
stTraverseWithSym f st = SymbolTable (stMap st) <$> traverse f' (stData st)
  where f' (sym, a) = (,) sym <$> f sym a

stTraverseWithSym_ :: Applicative t =>
                      (Ranged P.Symbol -> a -> t b) -> SymbolTable a -> t ()
stTraverseWithSym_ f st = Foldable.traverse_ f' (stData st)
  where f' (sym, a) = f sym a

stMapWithSymbol :: (Symbol -> a -> b) -> SymbolTable a -> SymbolTable b
stMapWithSymbol f st = SymbolTable (stMap st) $ listArray (bounds arr) (map f' (assocs arr))
  where arr = stData st
        f' (idx, (rpsym, a)) = (rpsym, f (Symbol idx) a)

data STBuilder a = STBuilder { stbMap :: Map.Map String Int
                             , stbList :: CList (Ranged P.Symbol, a)
                             }

stbEmpty :: STBuilder a
stbEmpty = STBuilder Map.empty clEmpty

stbAdd :: String -> STBuilder a -> LCRange ->
          Ranged P.Symbol -> a -> ErrorsOr (STBuilder a)
stbAdd what stb rng rsym a =
  if Map.member name (stbMap stb) then
    bad1 (Ranged rng $ "duplicate " ++ what ++ ": `" ++ name ++ "'.")
  else
    good $ STBuilder (Map.insert name len (stbMap stb)) (clCons (rsym, a) list)
  where name = P.symName (rangedData rsym)
        list = stbList stb
        len = clLen list

stbLastSymbol :: STBuilder a -> Symbol
stbLastSymbol stb = Symbol $ clLen (stbList stb) - 1

stbGet :: String -> LCRange -> P.Symbol -> STBuilder a -> ErrorsOr (Symbol, a)
stbGet what rng psym stb =
  do { idx <- mapGet what rng psym (stbMap stb)
     ; good $ (Symbol idx, snd $ clAt idx (stbList stb))
     }

stbToSymbolTable :: STBuilder a -> SymbolTable a
stbToSymbolTable stb = SymbolTable (stbMap stb) (clArray (stbList stb))
