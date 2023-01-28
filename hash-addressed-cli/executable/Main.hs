module Main (main) where

import Essentials
import HashAddressed.App.Command.Examples.Main

import Prelude (IO)

import qualified Control.Monad.Trans.Except as Except
import qualified Data.Either as Either
import qualified Options.Applicative as Options
import qualified System.Exit as Exit

main :: IO ()
main = do
    action <- Options.execParser command
    Except.runExceptT action >>= \case
        Either.Left x -> Exit.die x
        Either.Right () -> pure ()
