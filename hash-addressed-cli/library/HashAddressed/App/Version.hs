{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

module HashAddressed.App.Version
  (
    HashAddressed.App.Version.version
  )
  where

import Data.Version (showVersion)
import Paths_hash_addressed_cli as P (version)
import Prelude (String)

version :: String
version = showVersion P.version
