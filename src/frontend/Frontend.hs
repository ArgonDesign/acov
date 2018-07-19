module Frontend
  ( run
  ) where

import ErrorsOr (ErrorsOr, reportEO)
import Parser (parseScript)
import qualified Grouping
import qualified Symbols
import qualified Expressions
import qualified Width
import qualified Records

runPass :: FilePath -> (a -> ErrorsOr b) -> a -> IO b
runPass path pass a = reportEO path (pass a)

run :: FilePath -> IO [Records.Module]
run path = readFile path >>=
           runPass path (parseScript path) >>=
           runPass path Grouping.run >>=
           runPass path Symbols.run >>=
           runPass path Expressions.run >>=
           runPass path Width.run >>=
           runPass path Records.run
