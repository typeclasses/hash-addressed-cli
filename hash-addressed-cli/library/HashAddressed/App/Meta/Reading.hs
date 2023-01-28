module HashAddressed.App.Meta.Reading
  (
    readHashFunctionFromConfig,
  )
  where

import Essentials
import HashAddressed.App.HashFunction.Naming
import HashAddressed.App.Meta.Paths
import HashAddressed.App.Meta.Version

import Control.Monad.IO.Class (liftIO)
import HashAddressed.HashFunction (HashFunction)
import Prelude (FilePath, IO, String)

import qualified Control.Monad.Trans.Except as Except
import qualified Data.ByteString as Strict.ByteString
import qualified Data.Either as Either
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Ini as INI
import qualified Data.Text as Strict.Text
import qualified Data.Text.Encoding as Strict.Text

readHashFunctionFromConfig :: FilePath -> Except.ExceptT String IO HashFunction
readHashFunctionFromConfig storeDirectory = do
    bs <- liftIO $ Strict.ByteString.readFile (configFile storeDirectory)
    text <- case Strict.Text.decodeUtf8' bs of
        Either.Left _ -> Except.throwE $ "Invalid UTF-8 in config file " <> configFile storeDirectory
        Either.Right x -> pure x
    INI.Ini _ iniGlobals <- case INI.parseIni text of
        Either.Left _ -> Except.throwE $ "Invalid config file " <> configFile storeDirectory
        Either.Right ini -> pure ini
    let map = HashMap.fromList iniGlobals
    versionText <- case HashMap.lookup (Strict.Text.pack "version") map of
        Nothing -> Except.throwE $ "Missing version in config file " <> configFile storeDirectory
        Just x -> pure x
    _configVersion <- case Strict.Text.unpack versionText of
        "1" -> pure V1
        v -> Except.throwE $ "Unsupported config version " <> v <> " " <> configFile storeDirectory
    hashFunctionText <- case HashMap.lookup (Strict.Text.pack "hash function") map of
        Nothing -> Except.throwE $ "Missing hash function in config file " <> configFile storeDirectory
        Just x -> pure x
    case readHashFunctionText hashFunctionText of
        Nothing -> Except.throwE $ "Unsupported hash function " <> Strict.Text.unpack hashFunctionText
                <> " in config file " <> configFile storeDirectory
        Just x -> pure x
