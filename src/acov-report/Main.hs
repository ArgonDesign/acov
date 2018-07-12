module Main where

import Data.Monoid ((<>))
import Options.Applicative
import System.Exit
import System.Directory
import System.FilePath
import System.IO
import qualified Data.Map.Strict as Map

import Report

import Coverage
import qualified Frontend
import qualified Symbols as S
import qualified Width as W

import Ranged

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

getCoverage :: FilePath -> IO Coverage
getCoverage path =
  do { cover' <- readCoverage emptyCoverage path
     ; case cover' of
         Left err -> hPutStr stderr (err ++ "\n") >> exitFailure
         Right c -> return c
     }

data Reports = Reports { covers :: Map.Map (String, String) CoverReport }

procStmt :: PerModCoverage -> S.SymbolArray W.Module ->
            S.TLStmt -> Reports -> Reports
procStmt pmc modules (S.Cover rdsym clist) rpts =
  rpts { covers = Map.insert (modName, recName) cover (covers rpts) }
  where cover = mkCoverReport pmc modName recName recWidth clist
        S.DottedSymbol modsym recsym = rangedData rdsym
        modName = S.symbolName modules modsym
        W.Module mst _ = S.symbolData modules modsym
        recName = S.symbolName (W.mstRecords mst) recsym
        recWidth = S.symbolData (W.mstRecords mst) recsym

-- TODO: Handle crosses
procStmt pmc modules (S.Cross rdsyms) rpts = rpts

makeReports :: PerModCoverage -> S.SymbolArray W.Module -> [S.TLStmt] ->
               Reports
makeReports pmc mods = foldr (procStmt pmc mods) (Reports Map.empty)

dumpReports :: Reports -> Handle -> IO ()
dumpReports rpts handle =
  put ("<!DOCTYPE html><html><head>" ++
       "<meta charset='utf-8' />" ++
       "<title>Functional coverage report</title></head><body>") >>
  put "<h1>Single-variable coverage</h1>" >>
  Map.traverseWithKey dcr (covers rpts) >>
  put "</body></html>"
  where put = hPutStr handle
        dcr (modName, recName) = dumpCoverReport handle modName recName

run :: Args -> IO ()
run args = do { scr <- Frontend.run (input args)
              ; pmc <- swizzleCoverage <$> getCoverage (cover args)
              ; createDirectoryIfMissing False (odir args)
              ; withFile (odir args </> "index.html") WriteMode
                (dumpReports
                 (makeReports pmc (W.scrModules scr) (W.scrStmts scr)))
              ; return ()
              }

main :: IO ()
main = execParser mainInfo >>= run
