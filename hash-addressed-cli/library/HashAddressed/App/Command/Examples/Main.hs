module HashAddressed.App.Command.Examples.Main where

import Essentials
import HashAddressed.App.Command.Type
import HashAddressed.App.Command.Examples.Write
import HashAddressed.App.Command.Examples.Version
import HashAddressed.App.Command.Examples.Initialize

import qualified Options.Applicative as Options

command :: Command
command = Options.info (parser <**> Options.helper) $
    Options.progDesc "Hash-addressed file storage"
  where
    parser = Options.subparser $
        Options.command "version" versionCommand <>
        Options.command "initialize" initializeCommand <>
        Options.command "write" writeCommand
