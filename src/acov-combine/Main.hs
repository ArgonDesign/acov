module Main where

import Data.Monoid ((<>))
import Options.Applicative
import System.Exit

data Args = Args
  { input :: FilePath
  , odir  :: FilePath
  }

mainArgs :: Parser Args
mainArgs = Args
           <$> argument str ( metavar "input" <>
                              help "Input file" )
           <*> argument str ( metavar "odir" <>
                              help "Output directory" )

mainInfo :: ParserInfo Args
mainInfo = info (mainArgs <**> helper)
           ( fullDesc <>
             progDesc "Generate functional coverage bindings" <>
             header "acov - functional coverage bindings generator" )

run :: Args -> IO ()
run args = return ()

main :: IO ()
main = execParser mainInfo >>= run
