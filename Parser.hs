module Parser where

import Data.Functor.Identity (Identity)
import qualified Text.Parsec.Language as L
import qualified Text.Parsec.Token as T
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec

{-

  A coverage definition looks something like:

     // A module to collect xxx coverage
     module xxx (foo [10:0], bar [19:0], baz [1:0], qux) {
        trigger florbl;
        florbl = foo [0];

        when florbl {
          record foo;
        }

        when florbl record foo + bar[10:0] as foobar;

        record baz;
        record qux as qxx;
     }

     cover xxx.foo {0, 0x12, 2047};
     cover xxx.baz;
     cross xxx.foo xxx.baz;
-}
data DottedSymbol = DottedSymbol Symbol Symbol
  deriving Show

newtype CoverList = CoverList [Integer]
  deriving Show

data TLStmt = Module Symbol [Port] [Stmt]
            | Cover DottedSymbol (Maybe CoverList)
            | Cross [DottedSymbol]
  deriving Show

data Stmt = StmtTrigger Symbol
          | StmtAssign Symbol Expression
          | StmtRecord Expression (Maybe Symbol)
          | StmtWhen Symbol [Stmt]
  deriving Show

data UnOp = LogNot | BitNot
          | RedAnd | RedOr | RedNand | RedNor | RedXor | RedXNor
          | UPlus | UMinus
  deriving Show

data BinOp = Times | Divide | Modulo
           | Plus | Minus
           | LShift | RShift
           | Greater | GreaterEq | Less | LessEq
           | LogEq | LogNEq
           | CaseEq | CaseNEq
           | BitAnd
           | BitXor | BitXNor
           | BitOr
           | LogAnd
           | LogOr
  deriving Show

data Atom = AtomSym Symbol
          | AtomInt Integer
  deriving Show

data Expression = ExprAtom Atom
                | ExprParens Expression
                | ExprSlice Expression Slice
                | ExprConcat [Expression]
                | ExprReplicate Expression Expression
                | ExprUnOp UnOp Expression
                | ExprBinOp BinOp Expression Expression
                | ExprCond Expression Expression Expression
  deriving Show

{-
  Our first job is to set up a language definition and get the
  standard lexer etc.
-}
reservedNames :: [String]
reservedNames = [ "module"
                , "trigger"
                , "when"
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
newtype Symbol = Symbol String
  deriving Show

sym :: Parser Symbol
sym = Symbol <$> T.identifier lexer

integer :: Parser Integer
integer = T.integer lexer

commaSep :: Parser a -> Parser [a]
commaSep p = sepBy p (T.reservedOp lexer ",")

{-
  A "slice" is of the form [A:B] where we require both A and B to be
  non-negative integers.
-}
data Slice = Slice Integer Integer
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
data Port = Port Symbol (Maybe Slice)
  deriving Show

port :: Parser Port
port = do { name <- sym
          ; bits <- optionMaybe slice
          ; return $ Port name bits } <?> "port"

portList :: Parser [Port]
portList = T.parens lexer (commaSep port) <?> "port list"

{-
  In order to parse the internals of a "module", we need to parse
  expressions properly. We define the usual Verilog operators.

  The expression' parser parses everything that binds tighter than the
  ternary operator and the expression parser matches that too.
-}
atom :: Parser Atom
atom = (AtomSym <$> sym <?> "identifier") <|>
       (AtomInt <$> integer <?> "integer")

exprParens :: Parser Expression
exprParens = T.parens lexer expression >>= return . ExprParens

exprSlice :: Parser Expression
exprSlice = do { e <- ExprAtom <$> atom <?> "atom"
               ; s <- slice
               ; return $ ExprSlice e s }

exprConcat :: Parser Expression
exprConcat = T.braces lexer (commaSep expression) >>= return . ExprConcat

exprReplicate :: Parser Expression
exprReplicate =
  T.braces lexer (do { count <- expression'
                     ; value <- T.braces lexer expression <?> "replicand"
                     ; return $ ExprReplicate count value })

op :: String -> a -> Parser a
op str tx = T.reservedOp lexer str >> return tx

unop :: String -> UnOp -> Operator String () Identity Expression
unop str uo = Prefix $ op str (ExprUnOp uo)

binop :: String -> BinOp -> Operator String () Identity Expression
binop str bo = Infix (op str (ExprBinOp bo)) AssocLeft

term :: Parser Expression
term = (try (exprSlice <?> "sliced symbol")) <|>
       (ExprAtom <$> atom <?> "atom") <|>
       (exprParens <?> "parenthesized expression") <|>
       (try (exprConcat <?> "concatenation")) <|>
       (exprReplicate <?> "replicated expression")

table = [ [ unop "!" LogNot , unop "~" BitNot , unop "&" RedAnd
          , unop "|" RedOr , unop "~&" RedNand , unop "~|" RedNor
          , unop "^" RedXor , unop "~^" RedXNor , unop "^~" RedXNor ]
        , [ unop "+" UPlus , unop "-" UMinus ]
        , [ binop "*" Times , binop "/" Divide , binop "%" Modulo ]
        , [ binop "+" Plus , binop "-" Minus ]
        , [ binop "<<" LShift , binop ">>" RShift ]
        , [ binop ">" Greater , binop ">=" GreaterEq
          , binop "<" Less , binop "<=" LessEq ]
        , [ binop "==" LogEq , binop "!=" LogNEq ]
        , [ binop "===" CaseEq , binop "!==" CaseNEq ]
        , [ binop "&" BitAnd ]
        , [ binop "^" BitXor , binop "^~" BitXNor, binop "~^" BitXNor ]
        , [ binop "|" BitOr ]
        , [ binop "&&" LogAnd ]
        , [ binop "||" LogOr ]
        ]

expression' :: Parser Expression
expression' =
  buildExpressionParser table term

condTail :: Expression -> Parser Expression
condTail cond = do { T.reservedOp lexer "?" <?> "operator"
                   ; iftrue <- expression <?> "expression if true"
                   ; T.reservedOp lexer ":"
                   ; iffalse <- expression <?> "expression if false"
                   ; return $ ExprCond cond iftrue iffalse
                   }

expression :: Parser Expression
expression = do { a <- expression' <?> "expression"
                ; condTail a <|> return a }

{-
  Now that we have expressions, we can define statements, which are
  the internals of a module description. The parser is quite lax and
  allows any statement on the inside of a "when" etc. We'll strip out
  any silliness there later.
-}

trigger :: Parser Stmt
trigger = StmtTrigger <$> (T.reserved lexer "trigger" >>
                           (sym <?> "variable name for trigger") <*
                           T.semi lexer)

assignment :: Parser Stmt
assignment = do { x <- sym
                ; T.reservedOp lexer "="
                ; rhs <- expression <?> "expression for assignment RHS"
                ; T.semi lexer
                ; return $ StmtAssign x rhs }

record :: Parser Stmt
record = do { T.reserved lexer "record"
            ; e <- expression
            ; name <- optionMaybe (T.reserved lexer "as" >> sym)
            ; T.semi lexer
            ; return $ StmtRecord e name }

when :: Parser Stmt
when = do { T.reserved lexer "when"
          ; a <- sym
          ; b <- T.braces lexer (many statement) <|> (singleton <$> statement)
          ; return $ StmtWhen a b }
  where singleton a = [a]

statement :: Parser Stmt
statement = trigger <|> record <|> when <|> assignment

{-
  Parsing top-level statements
-}
module' :: Parser TLStmt
module' = do { T.reserved lexer "module"
         ; name <- sym
         ; pl <- portList
         ; stmts <- T.braces lexer (many statement)
         ; return $ Module name pl stmts
         }

dottedSymbol :: Parser DottedSymbol
dottedSymbol = do { mod <- sym
                  ; T.reservedOp lexer "."
                  ; var <- sym
                  ; return $ DottedSymbol mod var
                  }

coverList :: Parser CoverList
coverList = CoverList <$> T.braces lexer (commaSep integer)

cover :: Parser TLStmt
cover = do { T.reserved lexer "cover"
           ; name <- dottedSymbol
           ; clist <- optionMaybe coverList
           ; return $ Cover name clist
           }

cross :: Parser TLStmt
cross = T.reserved lexer "cover" >> (Cross <$> many1 dottedSymbol)

tlStmt :: Parser TLStmt
tlStmt = module' <|> cover <|> cross

script :: Parser [TLStmt]
script = many tlStmt
