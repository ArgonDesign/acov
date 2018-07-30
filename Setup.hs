{-# Language CPP #-}

import Distribution.Simple
import Distribution.PackageDescription (emptyHookedBuildInfo)
import System.Directory (createDirectoryIfMissing)
import System.Process (readProcess)

#ifndef MIN_VERSION_Cabal
#define MIN_VERSION_Cabal(a,b,c) 0
#endif

#if MIN_VERSION_Cabal(2,0,0)
import Distribution.Simple.BuildPaths (autogenPackageModulesDir)
#else
import Distribution.Simple.BuildPaths (autogenModulesDir)
#endif

#if MIN_VERSION_Cabal(2,0,0)
autogenDir = autogenPackageModulesDir
#else
autogenDir = autogenModulesDir
#endif

main = defaultMainWithHooks myHooks
  where myHooks = simpleUserHooks { postConf = acovPostConf }

-- Adapted from code at
-- http://www.hyperedsoftware.com/blog/entries/build-info-gen.html
acovPostConf args flags desc lbi =
  do { putStrLn ("Generating " ++ path ++ ".")
     ; desc <- readProcess "git" ["describe", "--dirty=-modified"] ""
     ; createDirectoryIfMissing True dir
     ; writeFile path $ unlines
       [ "module BuildVersion where"
       , "gitDescribe :: String"
       , "gitDescribe = " ++ show (init desc)
       ]
     }
  where dir = autogenDir lbi
        path = dir ++ "/BuildVersion.hs"
