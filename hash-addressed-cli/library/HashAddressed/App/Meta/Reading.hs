module HashAddressed.App.Meta.Reading
  (
    readHashFunctionFromConfig,
  )
  where

import Essentials
import HashAddressed.App.HashFunction.Naming
import HashAddressed.App.Meta.Paths
import HashAddressed.App.Meta.Version
import HashAddressed.App.Command.Type

import Control.Monad.IO.Class (liftIO)
import Prelude (FilePath)

import qualified Data.Sequence as Seq
import qualified Control.Monad.Trans.Except as Except
import qualified Data.ByteString as Strict.ByteString
import qualified Data.Either as Either
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Ini as INI
import qualified Data.Text as Strict.Text
import qualified Data.Text.Encoding as Strict.Text

readHashFunctionFromConfig :: FilePath -> CommandAction HashFunctionName
readHashFunctionFromConfig storeDirectory = do

    bs <- liftIO $ Strict.ByteString.readFile (configFile storeDirectory)

    text <- case Strict.Text.decodeUtf8' bs of
        Either.Left _ -> Except.throwE $ Seq.singleton $
            "Invalid UTF-8 in config file " <> configFile storeDirectory
        Either.Right x -> pure x

    INI.Ini _ iniGlobals <- case INI.parseIni text of
        Either.Left _ -> Except.throwE $ Seq.singleton $
            "Invalid config file " <> configFile storeDirectory
        Either.Right ini -> pure ini

    let map = HashMap.fromList iniGlobals

    versionText <- case HashMap.lookup (Strict.Text.pack "version") map of
        Nothing -> Except.throwE $ Seq.singleton $
            "Missing version in config file " <> configFile storeDirectory
        Just x -> pure x

    _configVersion <- case Strict.Text.unpack versionText of
        "1" -> pure V1
        v -> Except.throwE $ Seq.singleton $
            "Unsupported config version " <> v <> " " <> configFile storeDirectory

    hashFunctionText <- case HashMap.lookup (Strict.Text.pack "hash function") map of
        Nothing -> Except.throwE $ Seq.singleton $
            "Missing hash function in config file " <> configFile storeDirectory
        Just x -> pure x

    case readHashFunctionText hashFunctionText of
        Nothing -> Except.throwE $ Seq.singleton $
            "Unsupported hash function " <> Strict.Text.unpack hashFunctionText
            <> " in config file " <> configFile storeDirectory
        Just x -> pure x
