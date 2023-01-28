module HashAddressed.App.Meta.Paths
  (
    metaDirectory, configFile,
  )
  where

import Prelude (FilePath)
import System.FilePath ((</>))

metaDirectory :: FilePath -> FilePath
metaDirectory storeDirectory = storeDirectory </> ".hash-addressed"

configFile :: FilePath -> FilePath
configFile storeDirectory = metaDirectory storeDirectory </> "config"
