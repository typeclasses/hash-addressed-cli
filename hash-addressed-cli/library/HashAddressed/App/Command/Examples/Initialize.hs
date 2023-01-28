module HashAddressed.App.Command.Examples.Initialize
  (
    initializeCommand,
  )
  where

import Essentials
import HashAddressed.App.Command.Type
import HashAddressed.App.HashFunction.Options
import HashAddressed.App.Meta.Initialization
import HashAddressed.App.Verbosity.Options

import qualified Options.Applicative as Options

initializeCommand :: Command
initializeCommand =  Options.info (parser <**> Options.helper) $
    Options.progDesc "Initialize a hash-addressed store"
  where
    parser :: Options.Parser (CommandAction ())
    parser = do
        optStoreDirectory <- Options.strOption $ Options.long "directory" <>
            Options.help "Where the hash-addressed files are located"

        optHashFunction <- Options.option hashFunctionRead $
            Options.long "hash-function" <>
            Options.help hashFunctionInstructions

        optVerbosity <- verbosityOption

        pure $ tryInitializeStore CreateNew optVerbosity optHashFunction
            optStoreDirectory
