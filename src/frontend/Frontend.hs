module Frontend
  ( run
  ) where

import ErrorsOr (ErrorsOr, reportEO)
import Parser (parseScript)
import qualified When
import qualified Symbols
import qualified Expressions
import qualified Width

runPass :: FilePath -> (a -> ErrorsOr b) -> a -> IO b
runPass path pass a = reportEO path (pass a)

run :: FilePath -> IO Width.Script
run path = readFile path >>=
           runPass path (parseScript path) >>=
           runPass path When.run >>=
           runPass path Symbols.run >>=
           runPass path Expressions.run >>=
           runPass path Width.run
