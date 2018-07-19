module Parser
  ( Symbol(..)
  , symName
  , Module(..)
  , Port(..)
  , Statement(..)
  , Expression(..)
  , CoverList(..)
  , Slice(..)
  , Atom(..)
  , parseScript

  -- Exported just for testing
  , sym

  ) where

import Control.Applicative ((<*))
import Data.Char (digitToInt)
import Data.Functor ((<$>))
import Data.Functor.Identity (Identity)
import qualified Text.Parsec.Language as L
import qualified Text.Parsec.Token as T
import Text.Parsec
import Text.Parsec.Error
import Text.Parsec.Expr
import Text.Parsec.Pos
import Text.Parsec.String

import ErrorsOr
import Ranged
import Operators
import VInt

{-
  A coverage definition looks something like:

    // A module to collect xxx coverage
    module xxx (foo [10:0], bar [19:0], baz [1:0], qux) {
       when (foo [0]) {
         group {
           record foo;
           record foo + bar[10:0] as foobar;
           cover foo {0, 12, 2047};
           cover foobar {0, 1};
           cross foo foobar;
         }
       }

       group {
         record qux;
         cover qux;
       }
    }

  In the parsing stage, we allow group {} and when () {} to nest
  arbitrarily. We'll tighten stuff up in the next pass.

-}
newtype Symbol = Symbol String
  deriving (Show, Eq, Ord)

symName :: Symbol -> String
symName (Symbol name) = name

data Module = Module (Ranged Symbol) [Ranged Port] [Ranged Statement]

data Port = Port Symbol (Maybe (Ranged Slice))

data Statement = Record (Ranged Expression) (Maybe (Ranged Symbol))
               | Cover (Ranged Symbol) (Maybe CoverList)
               | Cross [Ranged Symbol]
               | When (Ranged Expression) [Ranged Statement]
               | Group [Ranged Statement]

newtype CoverList = CoverList [Ranged VInt]

data Atom = AtomSym Symbol
          | AtomInt VInt

data Expression = ExprAtom Atom
                | ExprParens (Ranged Expression)
                | ExprSel (Ranged Expression)
                  (Ranged Expression) (Maybe (Ranged Expression))
                | ExprConcat [Ranged Expression]
                | ExprReplicate (Ranged Expression) (Ranged Expression)
                | ExprUnOp (Ranged UnOp) (Ranged Expression)
                | ExprBinOp (Ranged BinOp)
                  (Ranged Expression) (Ranged Expression)
                | ExprCond (Ranged Expression)
                  (Ranged Expression) (Ranged Expression)

{-
  Our first job is to set up a language definition and get the
  standard lexer etc.
-}
reservedNames :: [String]
reservedNames = [ "module"
                , "when"
                , "group"
                , "record"
                , "as"
                , "cover"
                , "cross"
                ]

reservedOpNames = [ ";" , "," , ":" , "=" , "."
                  , "!" , "~" , "&" , "|" , "~&" , "~|"
                  , "^" , "~^" , "^~"
                  , "+" , "-"
                  , "*" , "/" , "%"
                  , "<<" , ">>"
                  , ">" , ">=" , "<" , "<="
                  , "==" , "!=" , "===" , "!=="
                  , "&&" , "||"
                  ]

language :: L.LanguageDef ()
language = L.emptyDef { T.commentLine = "//"
                      , T.reservedNames = reservedNames
                      , T.reservedOpNames = reservedOpNames
                      }

lexer :: T.TokenParser ()
lexer = T.makeTokenParser language

{-
  Use the lexer to define parsers for things like identifiers, integers etc.
-}
sym :: Parser Symbol
sym = Symbol <$> T.identifier lexer

semi :: Parser ()
semi = T.semi lexer >> return ()

commaSep :: Parser a -> Parser [a]
commaSep p = sepBy p (T.reservedOp lexer ",")

{-
  An integer needs to use the funky 8'h123 syntax, rather than the
  Haskell-like syntax ("o755") that you get from Parsec by default.
-}

genDigit :: Parser Char -> Parser Char
genDigit p = skipMany (char '_') >> p

uint :: Integer -> Parser Char -> Parser Integer
uint base digitParser = do { digits <- many1 (genDigit digitParser)
                           ; let n = foldl acc 0 digits
                           ; seq n $ return n
                           }
  where acc x c = base * x + toInteger (digitToInt c)

baseVInt :: Maybe Integer -> Parser VInt
baseVInt w = do { char '\''
                ; s <- option False (char 's' >> return True)
                ; n <- ((char 'h' >> uint 16 hexDigit) <|>
                        (char 'd' >> uint 10 digit) <|>
                        (char 'o' >> uint 8 octDigit) <|>
                        (char 'b' >> uint 2 binDigit))
                ; case makeVInt w s n of
                    Left err -> fail err
                    Right vint -> return vint
                }
  where binDigit = ((char '0' <|> char '1') <?> "binary digit")

integer :: Parser VInt
integer = T.lexeme lexer
          ((baseVInt Nothing) <|>
            do { d <- uint 10 digit ;
                 option (basicVInt d) (baseVInt (Just d)) })

{-
  A "slice" is of the form [A:B] where we require both A and B to be
  non-negative integers.
-}
data Slice = Slice VInt VInt
  deriving Show

slice :: Parser Slice
slice = (T.brackets lexer $
         do { lhs <- integer
            ; T.reservedOp lexer ":"
            ; rhs <- integer
            ; return $ Slice lhs rhs
            }) <?> "slice"

{-
  To parse the port list of a module, we need a comma separated list
  of identifiers, possibly with slices.
-}
port :: Parser Port
port = do { name <- sym
          ; bits <- optionMaybe (rangedParse slice)
          ; return $ Port name bits } <?> "port"

portList :: Parser [Ranged Port]
portList = T.parens lexer (commaSep $ rangedParse port) <?> "port list"

sourcePosToLCPos :: SourcePos -> LCPos
sourcePosToLCPos sp = LCPos (sourceLine sp) (sourceColumn sp)

sourcePosToLCRange :: SourcePos -> LCRange
sourcePosToLCRange sp = LCRange lcp lcp
  where lcp = sourcePosToLCPos sp

rangedParse :: Parser a -> Parser (Ranged a)
rangedParse p = do { lc0 <- sourcePosToLCPos <$> getPosition
                   ; a <- p
                   ; lc1 <- sourcePosToLCPos <$> getPosition
                   ; return $ Ranged (LCRange lc0 lc1) a
                   }

{-
  In order to parse the internals of a "module", we need to parse
  expressions properly. We define the usual Verilog operators.

  The expression' parser parses everything that binds tighter than the
  ternary operator and the expression parser matches that too.
-}
atom :: Parser Atom
atom = (AtomSym <$> sym <?> "identifier") <|>
       (AtomInt <$> integer <?> "integer")

exprAtom :: Parser Expression
exprAtom = ExprAtom <$> atom <?> "atom"

exprParens :: Parser Expression
exprParens = T.parens lexer expression >>= return . ExprParens

exprSelInternals :: Parser (Ranged Expression, Maybe (Ranged Expression))
exprSelInternals =
  do { top <- expression
     ; bot <- optionMaybe (T.reservedOp lexer ":" >> expression)
     ; return (top, bot)
     }

-- Matches foo [a : b] or foo [a]
exprSel :: Parser Expression
exprSel = do { e <- rangedParse exprAtom
             ; (t, b) <- T.brackets lexer exprSelInternals
             ; return $ ExprSel e t b
             }

exprConcat :: Parser Expression
exprConcat = T.braces lexer (commaSep expression) >>= return . ExprConcat

exprReplicate :: Parser Expression
exprReplicate =
  braces (do { count <- expression'
             ; value <- braces expression <?> "replicand"
             ; return $ ExprReplicate count value })
  where braces = T.braces lexer

rangedOp :: String -> a -> Parser (Ranged a)
rangedOp str a = rangedParse (T.reservedOp lexer str >> return a)

addUnOp :: Ranged UnOp -> Ranged Expression -> Ranged Expression
addUnOp ruo re = wideRange ruo re (ExprUnOp ruo re)

unop :: String -> UnOp -> Operator String () Identity (Ranged Expression)
unop s uo = Prefix (rangedOp s uo >>= return . addUnOp)

addBinOp :: Ranged BinOp ->
            Ranged Expression -> Ranged Expression -> Ranged Expression
addBinOp rbo re0 re1 = wideRange re0 re1 (ExprBinOp rbo re0 re1)

binop :: String -> BinOp -> Operator String () Identity (Ranged Expression)
binop s bo = Infix (rangedOp s bo >>= return . addBinOp) AssocLeft

term :: Parser Expression
term = (try exprSel) <|> exprAtom <|>
       (exprParens <?> "parenthesized expression") <|>
       (try (exprConcat <?> "concatenation")) <|>
       (exprReplicate <?> "replicated expression")

table = [ [ unop "!" LogNot , unop "~" BitNot , unop "&" RedAnd
          , unop "|" RedOr , unop "~&" RedNand , unop "~|" RedNor
          , unop "^" RedXor , unop "~^" RedXnor , unop "^~" RedXnor ]
        , [ unop "+" UPlus , unop "-" UMinus ]
        , [ binop "*" Times , binop "/" Divide , binop "%" Modulo ]
        , [ binop "+" Plus , binop "-" Minus ]
        , [ binop "<<" LShift , binop ">>" RShift ]
        , [ binop ">" Greater , binop ">=" GreaterEq
          , binop "<" Less , binop "<=" LessEq ]
        , [ binop "==" LogEq , binop "!=" LogNeq ]
        , [ binop "===" CaseEq , binop "!==" CaseNeq ]
        , [ binop "&" BitAnd ]
        , [ binop "^" BitXor , binop "^~" BitXnor, binop "~^" BitXnor ]
        , [ binop "|" BitOr ]
        , [ binop "&&" LogAnd ]
        , [ binop "||" LogOr ]
        ]

expression' :: Parser (Ranged Expression)
expression' =
  buildExpressionParser table (rangedParse term)

condTail :: Ranged Expression -> Parser (Ranged Expression)
condTail cond = do { T.reservedOp lexer "?" <?> "operator"
                   ; e0 <- expression <?> "expression if true"
                   ; T.reservedOp lexer ":"
                   ; e1 <- expression <?> "expression if false"
                   ; return $ wideRange cond e1 (ExprCond cond e0 e1)
                   }

expression :: Parser (Ranged Expression)
expression = do { a <- expression' <?> "expression"
                ; condTail a <|> return a }

statement :: Parser (Ranged Statement)
statement =
  rangedParse $
  ((group <|> when <|> cross <|> cover <|> record) <?> "statement")

group :: Parser Statement
group = T.reserved lexer "group" >>
        (Group <$> T.braces lexer (many1 statement))

when :: Parser Statement
when = do { T.reserved lexer "when"
           ; guard <- T.parens lexer expression
           ; stmts <- T.braces lexer (many1 statement)
           ; return $ When guard stmts
           }

cross :: Parser Statement
cross = T.reserved lexer "cross" >>
        (Cross <$> many1 (rangedParse sym)) <* semi

coverList :: Parser CoverList
coverList = CoverList <$> T.braces lexer (commaSep $ rangedParse integer)

cover :: Parser Statement
cover = do { T.reserved lexer "cover"
           ; name <- rangedParse sym
           ; clist <- optionMaybe coverList
           ; semi
           ; return $ Cover name clist
           }

record :: Parser Statement
record = do { T.reserved lexer "record"
            ; e <- expression
            ; name <- optionMaybe (T.reserved lexer "as" >> rangedParse sym)
            ; semi
            ; return $ Record e name }

module' :: Parser Module
module' = do { T.reserved lexer "module"
             ; name <- rangedParse sym
             ; ports <- portList
             ; stmts <- T.braces lexer (many1 statement)
             ; return $ Module name ports stmts
             }

script :: Parser [Module]
script = T.whiteSpace lexer >> many module' <* eof

makeErrors :: ParseError -> Ranged String
makeErrors pe = Ranged (sourcePosToLCRange (errorPos pe)) $
                showErrorMessages "or" "unknown parse error"
                                  "expecting" "unexpected" "end of input"
                                  (errorMessages pe)

parseScript :: FilePath -> String -> ErrorsOr [Module]
parseScript path str =
  case parse script path str of
    Left pe -> bad1 (makeErrors pe)
    Right stmts -> good stmts
