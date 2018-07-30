module Main where

import Data.Monoid ((<>))
import Options.Applicative

import BuildVersion (gitDescribe)
import qualified Frontend
import qualified Verilog


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
             header ("acov (" ++ gitDescribe ++
                     ") - functional coverage bindings generator") )

run :: Args -> IO ()
run args = Frontend.run (input args) >>= Verilog.run (odir args)

main :: IO ()
main = execParser mainInfo >>= run
