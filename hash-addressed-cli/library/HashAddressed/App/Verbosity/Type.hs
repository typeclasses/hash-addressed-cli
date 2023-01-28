module HashAddressed.App.Verbosity.Type
  (
    Verbosity (..),
  )
  where

import Essentials

data Verbosity = Verbosity{ quiet :: Bool, verbose :: Bool }
    deriving stock (Eq, Ord)
