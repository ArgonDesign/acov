module Coverage
  ( Coverage
  , emptyCoverage
  , readCoverage
  , dumpCoverage
  ) where

import qualified Control.Exception as CE
import Data.IntSet (IntSet , fromList , toList)
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Semigroup ((<>))
import System.IO
import Text.Parsec
import Text.Parsec.Error
import Text.Parsec.String
import qualified Text.Parsec.Language as L
import qualified Text.Parsec.Token as T

{-
  The type for coverage data is just a deeply nested set of maps.
-}
type VarCoverage = Map.Map String IntSet
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

data Record = Record String (String, String) IntSet

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

vals :: CParser IntSet
vals = fromList <$>
       (T.braces lexer $ sepBy int (T.reservedOp lexer ","))

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
newVC :: String -> IntSet -> VarCoverage
newVC = Map.singleton

updateVC :: VarCoverage -> String -> IntSet -> VarCoverage
updateVC vc var vals = Map.alter (Just . change) var vc
  where change Nothing = vals
        change (Just vals') = vals' <> vals

newSC :: (String, String) -> IntSet -> ScopeCoverage
newSC (mod, var) vals = Map.singleton mod $ newVC var vals

updateSC :: ScopeCoverage -> (String, String) -> IntSet -> ScopeCoverage
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
dumpIntSet :: IntSet -> IO ()
dumpIntSet vals = 
  putStr $ intercalate ", " (map show (toList vals))

dumpVar :: String -> (String, IntSet) -> IO ()
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
