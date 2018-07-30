module Raw
  ( Coverage
  , covCount
  , emptyCoverage
  , getModData
  , updateCoverage
  , ModData
  , traverseMD
  , ScopeData
  , sdMaxKey
  , sdGetGroup
  ) where

import Control.Applicative ((<*), Applicative)
import Control.Exception.Base
import qualified Control.Exception as CE
import Control.Monad

import Data.Char (digitToInt)
import Data.Functor ((<$>))
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Monoid ((<>))
import qualified Data.Set as Set

import Numeric (showHex)
import System.IO

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Error
import Text.Parsec.String
import qualified Text.Parsec.Language as L
import qualified Text.Parsec.Token as T

type IntegerSet = Set.Set Integer

{-
  Here are the fundamental types for the coverage data.

  ScopeData is the coverage data for a single scope. It is a map from
  group index to a set of integer values seen.
-}
newtype ScopeData = ScopeData (Map.Map Int IntegerSet)

sdMaxKey :: ScopeData -> Int
sdMaxKey (ScopeData map) = if Map.null map then (-1) else fst $ Map.findMax map

sdGetGroup :: ScopeData -> Int -> IntegerSet
sdGetGroup (ScopeData map) n = Map.findWithDefault Set.empty n map

{-
  ModData is the coverage data for a module (possibly with multiple
  instantiations). It is a map from scope name to associated data.
-}
newtype ModData = ModData (Map.Map String ScopeData)

emptyMD = ModData Map.empty

traverseMD :: Applicative t =>
              (String -> ScopeData -> t a) -> ModData -> t (Map.Map String a)
traverseMD f (ModData map) = Map.traverseWithKey f map

{-
  Coverage is the whole shebang. It is a map from module name to
  associated data, together with a counter that counts how many tests'
  data it contains.
-}
data Coverage = Coverage { covCount :: Int
                         , covMap :: Map.Map Integer ModData
                         }

emptyCoverage = Coverage 0 Map.empty

getModData :: Integer -> Coverage -> ModData
getModData modname cov = Map.findWithDefault emptyMD modname (covMap cov)

{-
  Let's define a parser for coverage logs.

  The first parsing pass just generates statements, which either set
  the module, set the scope or record a group.

  A record is the group number, then a list of values (parsed into
  integers)
-}
data Statement = Module Integer
               | Scope String
               | Record Int IntegerSet

language :: L.LanguageDef ()
language = L.emptyDef { T.commentLine = "#" }

lexer :: T.TokenParser ()
lexer = T.makeTokenParser language

hex :: Parser Integer
hex = do { n <- foldl acc 0 <$> (T.lexeme lexer (many1 hexDigit))
         ; seq n $ return n
         }
  where acc x c = 16 * x + toInteger (digitToInt c)

parseGrp :: Parser Int
parseGrp = do { integer <- hex
              ; if integer > toInteger (maxBound :: Int) then
                  fail "Group index overflows an Int."
                else
                  return ()
              ; return $ fromInteger integer
              }

vals :: Parser IntegerSet
vals = Set.fromList <$> (T.braces lexer $ sepBy hex (T.reservedOp lexer ","))

sym :: Parser String
sym = T.identifier lexer

colon :: Parser ()
colon = T.reservedOp lexer ":"

dot :: Parser ()
dot = T.reservedOp lexer "."

parseMod :: Parser Statement
parseMod = T.reserved lexer "MODULE" >> colon >> (Module <$> hex)

parseScope :: Parser Statement
parseScope = T.reserved lexer "SCOPE" >> colon >>
             (Scope . (intercalate ".") <$> (sepBy sym dot))

parseRecord :: Parser Statement
parseRecord = do { g <- parseGrp
                 ; colon
                 ; Record g <$> vals
                 }

statement :: Parser Statement
statement = (parseMod <|> parseScope <|> parseRecord) <?> "statement"

script :: Parser (Integer, [Statement])
script = do { T.whiteSpace lexer
            ; hash <- hex
            ; stmts <- many1 statement
            ; eof
            ; return (hash, stmts)
            }

parseScript :: SourceName -> String -> Either String (Integer, [Statement])
parseScript name contents =
  case parse script name contents of
    Left err ->
      Left $
      show (errorPos err) ++
      showErrorMessages "or" "unknown parse error"
                        "expecting" "unexpected" "end of input"
                        (errorMessages err)
    Right res -> Right res

{-
  Now we know how to parse a script into a list of statements, we can
  use them to update an existing coverage map.

  This might go wrong, because at the start of the list of statements,
  we have no ambient module and at the start of each module, we have
  no ambient scope. We're not putting much work into error reporting
  though - this should only fail if cover.cc generates rubbish for
  some reason.

  To track this, we have local state that we fold with an Either
  monad.
-}
type ScrState = (Maybe Integer, Maybe String)

takeStatement :: (ScrState, Coverage) -> Statement ->
                 Either String (ScrState, Coverage)

takeStatement ((_, _), cov) (Module mod) = return $ ((Just mod, Nothing), cov)

takeStatement ((smod, _), cov) (Scope scp) =
  case smod of
    Nothing -> Left $ "Cannot set scope to " ++ scp ++ " without ambient module."
    Just _ -> return $ ((smod, Just scp), cov)

takeStatement ((smod, ssc), cov) (Record grp vals) =
  case ssc of
    Nothing -> Left $ "Cannot record group without ambient scope."
    Just sc ->
      assert (isJust smod) $
      return ((smod, ssc), updCoverage (fromJust smod) sc grp vals cov)

updCoverage :: Integer -> String -> Int -> IntegerSet -> Coverage -> Coverage
updCoverage mod scope grp vals cov =
  cov { covMap = Map.alter (Just . (updMD scope grp vals)) mod (covMap cov) }

updMD :: String -> Int -> IntegerSet -> Maybe ModData -> ModData
updMD scope grp vals Nothing = newMD scope grp vals
updMD scope grp vals (Just (ModData map)) =
  ModData $ Map.alter (Just . (updSD grp vals)) scope map

newMD :: String -> Int -> IntegerSet -> ModData
newMD scope grp vals = ModData $ Map.singleton scope $ newSD grp vals

updSD :: Int -> IntegerSet -> Maybe ScopeData -> ScopeData
updSD grp vals Nothing = newSD grp vals
updSD grp vals (Just (ScopeData map)) =
  ScopeData $ Map.alter (Just . (updGrp vals)) grp map

newSD :: Int -> IntegerSet -> ScopeData
newSD grp vals = ScopeData $ Map.singleton grp vals

updGrp :: IntegerSet -> Maybe IntegerSet -> IntegerSet
updGrp vals Nothing = vals
updGrp vals (Just vals') = vals <> vals'

takeScript :: Coverage -> Int ->
              (Integer, [Statement]) -> Either String Coverage
takeScript cov hash (hash', stmts) =
  do { if toInteger hash /= hash' then
         Left ("Hash code in file is 0x" ++ showHex hash' "" ++
               ", which doesn't match the expected 0x" ++ showHex hash "" ++
               ".")
       else
         Right ()
     ; cov' <- snd <$> foldM takeStatement ((Nothing, Nothing), cov) stmts
     ; return $ cov' { covCount = covCount cov + 1 }
     }

takeContents :: Coverage -> Int -> FilePath -> String -> (Coverage, Maybe String)
takeContents cov hash path contents =
  case parseScript path contents >>= takeScript cov hash of
    Left err -> (cov, Just $ show path ++ ": Ignoring file. " ++ err)
    Right cov' -> (cov', Nothing)

updateCoverage :: Coverage -> Int -> FilePath -> IO (Coverage, Maybe String)
updateCoverage cov hash path =
  do { res <- (CE.try $ readFile path) :: IO (Either CE.IOException String)
     ; return $ case res of
         Left err -> (cov, Just $ show err)
         Right str -> takeContents cov hash path str
     }
