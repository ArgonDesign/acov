module Main where

import Control.Monad
import Data.Monoid ((<>))
import Options.Applicative
import System.Directory (createDirectoryIfMissing, copyFile)
import System.FilePath ((</>), takeFileName)
import System.Exit
import System.IO

import BuildVersion (gitDescribe)
import Paths_acov (getDataFileName)

import qualified Raw
import qualified Merge
import qualified CountPass
import Report

import qualified Frontend

data Args = Args
  { input :: FilePath
  , odir  :: FilePath
  , ignoreHash :: Bool
  }

mainArgs :: Parser Args
mainArgs = Args
           <$> argument str ( metavar "input" <>
                              help "Input file" )
           <*> argument str ( metavar "odir" <>
                              help "Output directory" )
           <*> switch ( long "ignore-hash" <> help "Whether to ignore report hashes" )

mainInfo :: ParserInfo Args
mainInfo = info (mainArgs <**> helper)
           ( fullDesc <>
             progDesc "Report functional coverage" <>
             header ("acov-report (" ++ gitDescribe ++
                     ") - functional coverage reporter") )

reportRecov :: (Raw.Coverage, Maybe String) -> IO Raw.Coverage
reportRecov (cov, Just str) = hPutStr stderr ("Warning: " ++ str ++ "\n") >>
                            return cov
reportRecov (cov, Nothing) = return cov

reportFatal :: Either String a -> IO a
reportFatal (Left err) = hPutStr stderr ("Error: " ++ err ++ "\n") >>
                       exitFailure
reportFatal (Right a) = return a

readCoverage :: Maybe Int -> IO Raw.Coverage
readCoverage hash =
  do { paths <- words <$> getContents
     ; if null paths then hPutStr stderr "Warning: No coverage files.\n"
       else return ()
     ; foldM f Raw.emptyCoverage paths
     }
  where f cov path = Raw.updateCoverage cov hash path >>= reportRecov

run :: Args -> IO ()
run args = do { (hash, mods) <- Frontend.run (input args)
              ; cov <- readCoverage
                       (if ignoreHash args then Nothing else Just hash)
              ; mcov <- reportFatal (Merge.mergeCoverage mods cov)
              ; createDirectoryIfMissing False (odir args)
              ; dataFiles <- withFile (odir args </> "index.html") WriteMode
                             (\ h -> report h $ CountPass.run mcov)
              ; mapM_ (installDataFile (odir args)) dataFiles
              ; exitSuccess
              }

installDataFile :: FilePath -> FilePath -> IO ()
installDataFile dst src = do { src' <- getDataFileName src
                             ; copyFile src' (dst </> (takeFileName src))
                             }

main :: IO ()
main = execParser mainInfo >>= run
