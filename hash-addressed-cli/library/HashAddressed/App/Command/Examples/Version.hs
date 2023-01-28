module HashAddressed.App.Command.Examples.Version
  (
    versionCommand,
  )
  where

import Essentials
import HashAddressed.App.Command.Type

import Control.Monad.IO.Class (liftIO)

import qualified Options.Applicative as Options
import qualified System.IO as IO

versionCommand :: Command
versionCommand = Options.info (parser <**> Options.helper) $
    Options.progDesc "Print version numbers"
  where
    parser :: Options.Parser (CommandAction ())
    parser = pure do
        liftIO $ IO.putStrLn $ "hash-addressed 1"
