module HashAddressed.App.Verbosity.Type where

import Essentials

data Verbosity = Verbosity{ quiet :: Bool, verbose :: Bool }
    deriving stock (Eq, Ord)
