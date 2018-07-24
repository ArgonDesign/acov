{-# Language CPP #-}

module Time
  ( stringTime
  ) where

import Data.Functor ((<$>))
import Data.Time.LocalTime
import Data.Time.Format

#if MIN_VERSION_time(1,5,0)
import Data.Time.Format (defaultTimeLocale)
#else
import System.Locale (defaultTimeLocale)
#endif

{-
  This module is needed because we want to support the old locales in
  GHC 8.4, but also the new ones for more modern GHCs.
-}

stringTime :: IO String
stringTime =
  formatTime defaultTimeLocale "%Y/%m/%d %H:%M" <$> getZonedTime
