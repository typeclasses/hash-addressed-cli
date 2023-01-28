module Main (main) where

import Essentials

import HashAddressed.App.Command.Examples.Main (mainCommand)
import Prelude (IO)

import qualified Control.Monad.Trans.Except as Except
import qualified Data.Either as Either
import qualified Options.Applicative as Options
import qualified System.Exit as Exit
import qualified System.IO as IO

main :: IO ()
main = do
    action <- Options.execParser mainCommand
    Except.runExceptT action >>= \case
        Either.Right () -> pure ()
        Either.Left xs -> do
            traverse_ (IO.hPutStrLn IO.stderr) xs
            Exit.exitFailure
