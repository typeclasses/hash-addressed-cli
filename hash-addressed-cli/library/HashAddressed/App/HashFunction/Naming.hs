module HashAddressed.App.HashFunction.Naming
  (
    HashFunctionName (..),
    readHashFunctionText,
    readHashFunctionString,
    showHashFunction,
    normalizeHashFunction,
    hashFunctions,
    resolveHashFunction,
  )
  where

import Essentials

import Data.Bool (not)
import HashAddressed.HashFunction (HashFunction)
import Prelude (String)

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Text as Strict
import qualified Data.Text as Strict.Text
import qualified HashAddressed.HashFunction as Hash

data HashFunctionName = SHA_256
    deriving Eq

resolveHashFunction :: HashFunctionName -> HashFunction
resolveHashFunction = \case
    SHA_256 -> Hash.sha256

hashFunctions :: [(String, HashFunctionName)]
hashFunctions = [(showHashFunction SHA_256, SHA_256)]

readHashFunctionText :: Strict.Text -> Maybe HashFunctionName
readHashFunctionText = Strict.Text.unpack >>> readHashFunctionString

readHashFunctionString :: String -> Maybe HashFunctionName
readHashFunctionString x = List.lookup x hashFunctions

showHashFunction :: HashFunctionName -> String
showHashFunction = \case
    SHA_256 -> "sha256"

normalizeHashFunction :: String -> String
normalizeHashFunction =
    List.map Char.toLower . List.filter (\x -> not (List.elem x " -_"))
