module Main where

import Control.Monad (foldM)
import Data.Monoid ((<>))
import Options.Applicative
import System.Exit
import System.IO

import Coverage

data Args = Args {}

mainInfo :: ParserInfo ()
mainInfo = info (pure () <**> helper)
           ( fullDesc <>
             (progDesc $
               "Combine coverage results." <>
               "This reads a list of log files from standard input, parses " <>
               "and combines them, then writes them to standard output.")
             <> header "acov-combine - functional coverage results combiner"
           )

takeCoverage :: Coverage -> FilePath -> IO Coverage
takeCoverage c fp = do { res <- readCoverage c fp
                       ; case res of
                           Left err ->
                             hPutStr stderr (err ++ "\n") >>
                             exitFailure
                           Right c' ->
                             return c'
                       }

main :: IO ()
main =
  execParser mainInfo >>
  do { inputs <- words <$> getContents
     ; if null inputs then hPutStr stderr "Warning: No input files.\n"
       else return ()
     ; foldM takeCoverage emptyCoverage inputs >>= dumpCoverage
     }
