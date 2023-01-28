module HashAddressed.App.Command.Examples.Main
  (
    mainCommand,
  )
  where

import Essentials
import HashAddressed.App.Command.Examples.Initialize
import HashAddressed.App.Command.Examples.Version
import HashAddressed.App.Command.Examples.Write
import HashAddressed.App.Command.Type

import qualified Options.Applicative as Options

mainCommand :: Command
mainCommand = Options.info (parser <**> Options.helper) $
    Options.progDesc "Hash-addressed file storage"
  where
    parser = Options.subparser $
        Options.command "version" versionCommand <>
        Options.command "initialize" initializeCommand <>
        Options.command "write" writeCommand
