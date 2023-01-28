module HashAddressed.App.HashFunction.Options
  (
    hashFunctionOption,
  )
  where

import Essentials
import HashAddressed.App.HashFunction.Naming

import HashAddressed.HashFunction (HashFunction)

import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Options.Applicative as Options

hashFunctionOption :: Options.Parser HashFunction
hashFunctionOption =
    Options.option read $ Options.long "hash-function" <> Options.help
      ("Choices: " <> choices <> "; space, dash, underscore, and letter case are ignored")
  where
    read = do
        string <- Options.str
        case List.lookup (normalizeHashFunction string) hashFunctions of
            Just x -> pure x
            Nothing -> Monad.fail $ "Unsupported hash function. Choices are: " <> choices
    choices = "[ SHA-256 ]"
