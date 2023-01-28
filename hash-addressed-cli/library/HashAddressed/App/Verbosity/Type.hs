module HashAddressed.App.Verbosity.Type
  (
    Verbosity (..),
  )
  where

import Essentials

-- | On non-zero exit code, @stderr@ messages are printed regardless of verbosity
data Verbosity =
    Verbosity
      { quiet :: Bool -- ^ Controls whether normal @stdout@ output is printed
      , verbose :: Bool -- ^ Controls whether chatty @stderr@ output is printed
      }
    deriving stock (Eq, Ord)
