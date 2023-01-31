module HashAddressed.App.HashFunction.Options
  (
    hashFunctionRead,
    hashFunctionInstructions,
  )
  where

import Essentials
import HashAddressed.App.HashFunction.Naming

import Prelude (String)

import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Options.Applicative as Options

hashFunctionRead :: Options.ReadM HashFunctionName
hashFunctionRead = do
    string <- Options.str
    case List.lookup (normalizeHashFunction string) hashFunctions of
        Just x -> pure x
        Nothing -> Monad.fail $ "Unsupported hash function. Choices are: " <> choices

choices :: String
choices = "[ SHA-256 ]"

hashFunctionInstructions :: String
hashFunctionInstructions = "Choices: " <> choices <>
    "; space, dash, underscore, and letter case are ignored"
