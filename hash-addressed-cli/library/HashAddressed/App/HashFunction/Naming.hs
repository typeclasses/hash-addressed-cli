module HashAddressed.App.HashFunction.Naming
  (
    readHashFunctionText,
    readHashFunctionString,
    showHashFunction,
    normalizeHashFunction,
    hashFunctions,
  )
  where

import Essentials

import Data.Bool (not)
import HashAddressed.HashFunction (HashFunction (SHA_256))
import Prelude (String)

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Text as Strict
import qualified Data.Text as Strict.Text

hashFunctions :: [(String, HashFunction)]
hashFunctions = [(showHashFunction SHA_256, SHA_256)]

readHashFunctionText :: Strict.Text -> Maybe HashFunction
readHashFunctionText = Strict.Text.unpack >>> readHashFunctionString

readHashFunctionString :: String -> Maybe HashFunction
readHashFunctionString x = List.lookup x hashFunctions

showHashFunction :: HashFunction -> String
showHashFunction = \case
    SHA_256 -> "sha256"

normalizeHashFunction :: String -> String
normalizeHashFunction =
    List.map Char.toLower . List.filter (\x -> not (List.elem x " -_"))
