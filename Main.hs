module Main where

import Options.Applicative
import Data.Semigroup ((<>))

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

run :: Args -> IO ()
run args = putStrLn (input args)

main :: IO ()
main = execParser mainInfo >>= run
