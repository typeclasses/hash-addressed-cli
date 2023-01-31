module HashAddressed.App.Meta.Initialization
  (
    defaultConfigINI, tryInitializeStore, initializeStore,
    InitializationType (..),
  )
  where

import Essentials
import HashAddressed.App.HashFunction.Naming
import HashAddressed.App.Meta.Paths
import HashAddressed.App.Verbosity.Printing
import HashAddressed.App.Verbosity.Type
import HashAddressed.App.Command.Type

import Control.Monad.IO.Class (liftIO)
import Prelude (FilePath)

import qualified Data.ByteString as Strict.ByteString
import qualified Data.Ini as INI
import qualified Data.Text as Strict.Text
import qualified Data.Text.Encoding as Strict.Text
import qualified System.Directory as Directory

defaultConfigINI :: HashFunctionName -> INI.Ini
defaultConfigINI optHashFunction = INI.Ini mempty
    [ (Strict.Text.pack "version", Strict.Text.pack "1")
    , (Strict.Text.pack "hash function",
      Strict.Text.pack (showHashFunction optHashFunction))]

tryInitializeStore :: InitializationType -> Verbosity -> HashFunctionName -> FilePath -> CommandAction ()
tryInitializeStore initializationType optVerbosity optHashFunction optStoreDirectory = do
    liftIO (Directory.doesPathExist (metaDirectory optStoreDirectory)) >>= \case
        False -> initializeStore optHashFunction optStoreDirectory
        True -> case initializationType of
            CreateNew -> initializeStore optHashFunction optStoreDirectory
            CreateIfNotPresent ->
                putVerboseLn optVerbosity "Store is already initialized."

data InitializationType = CreateNew | CreateIfNotPresent

{-| Assumes a store does not already exist -}
initializeStore :: HashFunctionName -> FilePath -> CommandAction ()
initializeStore optHashFunction optStoreDirectory = do
    liftIO $ Directory.createDirectoryIfMissing True (metaDirectory optStoreDirectory)
    liftIO $ Strict.ByteString.writeFile (configFile optStoreDirectory) $
        Strict.Text.encodeUtf8 (INI.printIni (defaultConfigINI optHashFunction))
