module HashAddressed.App.Command.Examples.Initialize where

import Essentials
import HashAddressed.App.Command.Type
import HashAddressed.App.Verbosity.Options
import HashAddressed.App.HashFunction.Options
import HashAddressed.App.Meta.Initialization

import qualified Options.Applicative as Options

initializeCommand :: Command
initializeCommand =  Options.info (parser <**> Options.helper) $
    Options.progDesc "Initialize a hash-addressed store"
  where
    parser = do
        optVerbosity <- verbosityOption
        optHashFunction <- hashFunctionOption
        optStoreDirectory <- Options.strOption $ Options.long "directory" <>
            Options.help "Where the hash-addressed files are located"
        pure do
            tryInitializeStore CreateNew optVerbosity
                optHashFunction optStoreDirectory
