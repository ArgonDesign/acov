module Frontend
  ( run
  ) where

import ErrorsOr (ErrorsOr, reportEO)
import Parser (parseScript)
import qualified Grouping
import qualified Symbols
import qualified Expressions
import qualified Width

runPass :: FilePath -> (a -> ErrorsOr b) -> a -> IO b
runPass path pass a = reportEO path (pass a)

run :: FilePath -> IO (Int, [Width.Module])
run path = readFile path >>=
           runPass path (parseScript path) >>=
           runPass path Grouping.run >>=
           runPass path Symbols.run >>=
           runPass path Expressions.run >>=
           runPass path Width.run
