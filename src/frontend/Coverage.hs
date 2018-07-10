module Coverage
  ( -- Used by acov-combine
    Coverage
  , emptyCoverage
  , readCoverage
  , dumpCoverage

  -- Used by acov-report
  , PerModCoverage
  , swizzleCoverage
  , pmcLookup
  ) where

import Control.Applicative ((<*))
import qualified Control.Exception as CE
import qualified Data.IntSet as IS
import Data.Functor ((<$>))
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Monoid ((<>))
import System.IO
import Text.Parsec
import Text.Parsec.Error
import Text.Parsec.String
import qualified Text.Parsec.Language as L
import qualified Text.Parsec.Token as T

{-
  The type for coverage data is just a deeply nested set of maps.
-}
type VarCoverage = Map.Map String IS.IntSet
type ScopeCoverage = Map.Map String VarCoverage

data Coverage = Coverage (Map.Map String ScopeCoverage)

emptyCoverage :: Coverage
emptyCoverage = Coverage Map.empty

{-
  Let's start by defining a parser for coverage logs. They match the
  following BNF:

    LOG     := LINE*
    LINE    := LINEX COMMENT | LINEX
    LINEX   := SCOPE | RECORD | ''
    COMMENT := '#' [^\n]*

    SCOPE   := 'SCOPE:' PATH
    PATH    := [a-zA-Z0-9_.]+

    RECORD  := RECNAME ':' VALS
    RECNAME := SYM '.' SYM
    SYM     := [a-zA-Z_][a-zA-Z0-9_]*
    VALS    := '{' NUMS '}'
    NUMS    := '' | NUMS1
    NUMS1   := NUM ',' NUMS1 | NUM

-}

type CState = Maybe String
type CParser = Parsec String CState

data Record = Record String (String, String) IS.IntSet

language :: L.LanguageDef CState
language = L.emptyDef { T.commentLine = "#" }

lexer :: T.TokenParser CState
lexer = T.makeTokenParser language

-- TODO: The error message here is pretty rubbish. :-/
int :: CParser Int
int = try $
      do { n <- T.integer lexer
         ; if n <= toInteger (maxBound :: Int) &&
              n >= toInteger (minBound :: Int) then
             return $ fromInteger n
           else
             fail $ "Integer " ++ show n ++ " is out of range."
         }

vals :: CParser IS.IntSet
vals = IS.fromList <$> (T.braces lexer $ sepBy int (T.reservedOp lexer ","))

sym :: CParser String
sym = T.identifier lexer

colon :: CParser ()
colon = T.reservedOp lexer ":"

dot :: CParser ()
dot = T.reservedOp lexer "."

recname :: CParser (String, String)
recname = do { a <- sym
             ; dot
             ; b <- sym
             ; return (a, b)
             }

recordAt :: Maybe String -> CParser Record
recordAt Nothing = fail "Cannot read record with no scope."
recordAt (Just scope) =
  do { name <- recname
     ; colon
     ; v <- vals
     ; return $ Record scope name v
     }

record :: CParser Record
record = getState >>= recordAt

scope :: CParser ()
scope = T.reserved lexer "SCOPE" >> colon >>
        sepBy sym dot >>= (putState . Just . intercalate ".")

line :: CParser (Maybe Record)
line = (scope >> return Nothing) <|> (Just <$> record)

script :: CParser [Record]
script = T.whiteSpace lexer >> (catMaybes <$> many line) <* eof

parseScript :: SourceName -> String -> Either String [Record]
parseScript name contents =
  case runParser script Nothing name contents of
    Left err ->
      Left $
      show (errorPos err) ++
      showErrorMessages "or" "unknown parse error"
                        "expecting" "unexpected" "end of input"
                        (errorMessages err)
    Right records -> Right records

parseFile :: FilePath -> IO (Either String [Record])
parseFile path =
  do { contents <-
       (CE.try $ readFile path) :: IO (Either CE.IOException String)
     ; return $ case contents of
         Left err -> Left $ show err
         Right str -> parseScript path str
     }

{-
  Now we know how to parse a file into a list of records, we can use a
  record to update coverage.
-}
newVC :: String -> IS.IntSet -> VarCoverage
newVC = Map.singleton

updateVC :: VarCoverage -> String -> IS.IntSet -> VarCoverage
updateVC vc var vals = Map.alter (Just . change) var vc
  where change Nothing = vals
        change (Just vals') = vals' <> vals

newSC :: (String, String) -> IS.IntSet -> ScopeCoverage
newSC (mod, var) vals = Map.singleton mod $ newVC var vals

updateSC :: ScopeCoverage -> (String, String) -> IS.IntSet -> ScopeCoverage
updateSC sc (mod, var) vals = Map.alter (Just . change) mod sc
  where change Nothing = newVC var vals
        change (Just vc) = updateVC vc var vals

updateCoverage :: Coverage -> Record -> Coverage
updateCoverage (Coverage scopes) (Record scope recname values) =
  Coverage (Map.alter (Just . change) scope scopes)
  where change Nothing = newSC recname values
        change (Just sc) = updateSC sc recname values

{-
  We can now combine the parser with updateCoverage to read in a file.
-}
readCoverage :: Coverage -> FilePath -> IO (Either String Coverage)
readCoverage c fp = parseFile fp >>= return . fmap (foldl' updateCoverage c)

{-
  Finally, we need to be able to dump the new coverage that we've got
-}
dumpIntSet :: IS.IntSet -> IO ()
dumpIntSet vals = 
  putStr $ intercalate ", " (map show (IS.toList vals))

dumpVar :: String -> (String, IS.IntSet) -> IO ()
dumpVar modname (var, vals) =
  putStr modname >> putStr "." >> putStr var >> putStr " : {" >>
  dumpIntSet vals >> putStr "}\n"

dumpVC :: (String, VarCoverage) -> IO ()
dumpVC (modname, vc) = mapM_ (dumpVar modname) (Map.toList vc)

dumpScope :: (String, ScopeCoverage) -> IO ()
dumpScope (name, sc) =
  putStr "SCOPE: " >> putStrLn name >> mapM_ dumpVC (Map.toList sc)

dumpCoverage :: Coverage -> IO ()
dumpCoverage (Coverage scopes) = mapM_ dumpScope (Map.toList scopes)

{-
  After reading in the coverage, we can swizzle the map around to get
  it in the form

    Module -> Scope -> Vars

  A ModCoverage instance is the coverage data for one module. A
  PerModCoverage instance is a map, keyed by module name, with a
  ModCoverage instance for each module.
-}
newtype ModCoverage = ModCoverage (Map.Map String VarCoverage)

mcNew :: String -> VarCoverage -> ModCoverage
mcNew scopeName vc = ModCoverage $ Map.singleton scopeName vc

mcSet :: String -> VarCoverage -> ModCoverage -> ModCoverage
mcSet scopeName vc (ModCoverage mc) = ModCoverage (Map.insert scopeName vc mc)

mcLookup :: ModCoverage -> String -> Map.Map String IS.IntSet
mcLookup (ModCoverage mc) var = Map.map get mc
  where get vc = case Map.lookup var vc of
                   Nothing -> IS.empty
                   Just vals -> vals

newtype PerModCoverage = PerModCoverage (Map.Map String ModCoverage)

pmcEmpty :: PerModCoverage
pmcEmpty = PerModCoverage Map.empty

pmcSet :: String -> String -> VarCoverage -> PerModCoverage -> PerModCoverage
pmcSet modName scopeName vc (PerModCoverage pmc) =
  PerModCoverage (Map.alter (Just . change) modName pmc)
  where change Nothing = mcNew scopeName vc
        change (Just mc) = mcSet scopeName vc mc

pmcLookup :: PerModCoverage -> String -> String -> Map.Map String IS.IntSet
pmcLookup (PerModCoverage pmc) mod var =
  case Map.lookup mod pmc of
    Nothing -> Map.empty
    Just mc -> mcLookup mc var

swizzleCoverage :: Coverage -> PerModCoverage
swizzleCoverage (Coverage scopes) = Map.foldrWithKey updPMC pmcEmpty scopes
  where updPMC scopeName sc pmc = Map.foldrWithKey (updPMC' scopeName) pmc sc
        updPMC' scopeName modName = pmcSet modName scopeName
