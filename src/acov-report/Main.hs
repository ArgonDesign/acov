module Main where

import Data.Monoid ((<>))
import Options.Applicative

import qualified Frontend

data Args = Args
  { input :: FilePath
  , cover :: FilePath
  , odir  :: FilePath
  }

mainArgs :: Parser Args
mainArgs = Args
           <$> argument str ( metavar "input" <>
                              help "Input file" )
           <*> argument str ( metavar "acov.log" <>
                              help "Coverage log" )
           <*> argument str ( metavar "odir" <>
                              help "Output directory" )

mainInfo :: ParserInfo Args
mainInfo = info (mainArgs <**> helper)
           ( fullDesc <>
             progDesc "Report functional coverage" <>
             header "acov-report - functional coverage reporter" )

run :: Args -> IO ()
run args = Frontend.run (input args) >> return ()

main :: IO ()
main = execParser mainInfo >>= run
