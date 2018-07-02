module Main where

import Data.Monoid ((<>))
import Options.Applicative
import System.Exit

import ErrorsOr (ErrorsOr, reportEO)
import Parser (parseScript)
import qualified When

data Args = Args
  { input :: FilePath
  }

mainArgs :: Parser Args
mainArgs = Args
           <$> argument str ( metavar "input" <>
                              help "Input file" )

mainInfo :: ParserInfo Args
mainInfo = info (mainArgs <**> helper)
           ( fullDesc <>
             progDesc "Generate functional coverage bindings" <>
             header "acov - functional coverage bindings generator" )

runPass :: FilePath -> (a -> ErrorsOr b) -> a -> IO b
runPass path pass a = reportEO path (pass a)

run :: Args -> IO ()
run args = readFile path >>=
           runPass path (parseScript path) >>=
           runPass path When.run >>
           exitSuccess
  where path = input args

main :: IO ()
main = execParser mainInfo >>= run

-- readFile (input args)
