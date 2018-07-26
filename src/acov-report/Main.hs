module Main where

import Control.Monad
import Data.Monoid ((<>))
import Options.Applicative
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Exit
import System.IO

import qualified Raw
import qualified Merge
import qualified CountPass
import Report

import qualified Frontend

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
             progDesc "Report functional coverage" <>
             header "acov-report - functional coverage reporter" )

reportErr :: Either String a -> IO a
reportErr (Left err) = hPutStr stderr ("Error: " ++ err ++ "\n") >>
                       exitFailure
reportErr (Right a) = return a

readCoverage :: IO Raw.Coverage
readCoverage =
  do { paths <- words <$> getContents
     ; if null paths then hPutStr stderr "Warning: No coverage files.\n"
       else return ()
     ; foldM f Raw.emptyCoverage paths
     }
  where f cov path = Raw.updateCoverage cov path >>= reportErr

run :: Args -> IO ()
run args = do { mods <- Frontend.run (input args)
              ; cov <- readCoverage
              ; mcov <- reportErr (Merge.mergeCoverage mods cov)
              ; createDirectoryIfMissing False (odir args)
              ; withFile (odir args </> "index.html") WriteMode
                (\ h -> report h $ CountPass.run mcov)
              ; exitSuccess
              }

main :: IO ()
main = execParser mainInfo >>= run
