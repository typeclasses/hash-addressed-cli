module Main (main) where

import Essentials

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bool (not, (||))
import HashAddressed.Directory (WriteResult (..), WriteType (..))
import HashAddressed.HashFunction (HashFunction (SHA_256))
import Prelude (IO, FilePath, String)
import System.FilePath ((</>))

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.ByteString as Strict
import qualified Data.ByteString as Strict.ByteString
import qualified Data.Char as Char
import qualified Data.Either as Either
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Ini as INI
import qualified Data.List as List
import qualified Data.Text as Strict
import qualified Data.Text as Strict.Text
import qualified Data.Text.Encoding as Strict.Text
import qualified HashAddressed.Directory
import qualified Options.Applicative as Options
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import qualified System.IO as IO

main :: IO ()
main = do
    action <- Options.execParser command
    Except.runExceptT action >>= \case
        Either.Left x -> Exit.die x
        Either.Right () -> pure ()

type Command = Options.ParserInfo (Except.ExceptT String IO ())

command :: Command
command = Options.info (parser <**> Options.helper) $
    Options.progDesc "Hash-addressed file storage"
  where
    parser = Options.subparser $
        Options.command "version" versionCommand <>
        Options.command "initialize" initializeCommand <>
        Options.command "write" writeCommand


---  The 'version' command  ---

versionCommand :: Command
versionCommand = Options.info (parser <**> Options.helper) $
    Options.progDesc "Print version numbers"
  where
    parser = pure do
        liftIO $ IO.putStrLn $ "hash-addressed 1"


---  The meta directory  ---

metaDirectory :: FilePath -> FilePath
metaDirectory storeDirectory = storeDirectory </> ".hash-addressed"

configFile :: FilePath -> FilePath
configFile storeDirectory = metaDirectory storeDirectory </> "config"

defaultConfigINI :: HashFunction -> INI.Ini
defaultConfigINI optHashFunction = INI.Ini mempty
    [ (Strict.Text.pack "version", Strict.Text.pack "1")
    , (Strict.Text.pack "hash function",
      Strict.Text.pack (showHashFunction optHashFunction))]

data Version = V1

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


---  The 'initialize' command  ---

initializeCommand :: Command
initializeCommand =  Options.info (parser <**> Options.helper) $
    Options.progDesc "Initialize a hash-addressed store"
  where
    parser = do
        optVerbosity <- verbosityOption
        optHashFunction <- hashFunctionOption
        optStoreDirectory <- Options.strOption $ Options.long "directory" <>
            Options.help "Where the hash-addressed files are located"
        pure do
            tryInitializeStore CreateNew optVerbosity
                optHashFunction optStoreDirectory

tryInitializeStore :: InitializationType -> Verbosity -> HashFunction -> FilePath
    -> Except.ExceptT String IO ()
tryInitializeStore initializationType optVerbosity optHashFunction optStoreDirectory = do
    liftIO (Directory.doesPathExist (metaDirectory optStoreDirectory)) >>= \case
        False -> initializeStore optHashFunction optStoreDirectory
        True -> case initializationType of
            CreateNew -> initializeStore optHashFunction optStoreDirectory
            CreateIfNotPresent ->
                putVerboseLn optVerbosity "Store is already initialized."

data InitializationType = CreateNew | CreateIfNotPresent

{-| Assumes a store does not already exist -}
initializeStore :: HashFunction -> FilePath -> Except.ExceptT String IO ()
initializeStore optHashFunction optStoreDirectory = do
    liftIO $ Directory.createDirectoryIfMissing True (metaDirectory optStoreDirectory)
    liftIO $ Strict.ByteString.writeFile (configFile optStoreDirectory) $
        Strict.Text.encodeUtf8 (INI.printIni (defaultConfigINI optHashFunction))


---  The 'write' command  ---

writeCommand :: Command
writeCommand = Options.info (parser <**> Options.helper) $ Options.progDesc
    "Copy from the standard input stream to a hash-addressed store"
  where
    parser = do
        optVerbosity <- verbosityOption
        optHashFunction <- Options.optional hashFunctionOption
        optStoreDirectory <- Options.strOption $ Options.long "target-directory" <>
            Options.help "Where the hash-addressed files are located"
        optInitializeStore <- Options.switch $ Options.long "initialize" <>
            Options.help "Set up a hash-addressed store if one does not already exist"
        optSourceFile <- Options.optional $ Options.strOption $ Options.long "source-file" <>
            Options.help "Path of file to copy to the store; if this option is not given, \
                \will read from standard input stream instead"
        pure do
            hashFunction <-
                case optInitializeStore of
                    True -> case optHashFunction of
                        Nothing -> Except.throwE $ "--initialize requires --hash-function"
                        Just hf -> do
                            Monad.when optInitializeStore $ tryInitializeStore CreateIfNotPresent
                                optVerbosity hf optStoreDirectory
                            pure hf
                    False -> do
                        configHashFunction <- readHashFunctionFromConfig optStoreDirectory
                        case optHashFunction of
                            Just hf | hf /= configHashFunction -> Except.throwE $
                                "--hash-function " <> showHashFunction hf <>
                                " does not match hash function " <> showHashFunction configHashFunction
                                <> " in " <> configFile optStoreDirectory
                            _ -> pure ()
                        pure configHashFunction

            putVerboseLn optVerbosity $ "The hash function is "
                <> showHashFunction hashFunction

            let store = HashAddressed.Directory.init hashFunction optStoreDirectory

            WriteResult{ contentAddressedFile, writeType } <- liftIO $ Resource.runResourceT @IO do

                input <- case optSourceFile of
                    Nothing -> pure IO.stdin
                    Just inputFile -> do
                        (_, h) <- Resource.allocate (IO.openBinaryFile inputFile IO.ReadMode) IO.hClose
                        pure h

                liftIO $ HashAddressed.Directory.writeStreaming store \(writeChunk :: Strict.ByteString -> m ()) -> do
                  let
                    loop :: m ()
                    loop = do
                      x <- liftIO $ Strict.ByteString.hGetSome input 4096
                      case Strict.ByteString.null x of
                          False -> writeChunk x *> loop
                          True -> pure ()
                  loop

            putVerboseLn optVerbosity case writeType of
                AlreadyPresent -> "The file was already present in the \
                    \store; no change was made."
                NewContent -> "One new file was added to the store."

            putNormalLn optVerbosity contentAddressedFile


---  Verbosity options  ---

data Verbosity = Verbosity{ quiet :: Bool, verbose :: Bool }
    deriving stock (Eq, Ord)

verboseOption :: Options.Parser Bool
verboseOption = Options.switch $ Options.long "verbose" <> Options.help
    "Print miscellaneous commentary to stderr"

quietOption :: Options.Parser Bool
quietOption = Options.switch $ Options.long "quiet" <> Options.help
    "Print nothing but fatal errors"

verbosityOption :: Options.Parser Verbosity
verbosityOption = do
    quiet <- quietOption
    verbose <- verboseOption
    pure Verbosity{ quiet, verbose }

putNormalLn :: MonadIO m => Verbosity -> String -> m ()
putNormalLn Verbosity{ quiet } x =
    if quiet then pure () else liftIO $ IO.hPutStrLn IO.stdout x

putVerboseLn :: MonadIO m => Verbosity -> String -> m ()
putVerboseLn Verbosity{ quiet, verbose } x =
    if quiet || not verbose then pure () else liftIO $ IO.hPutStrLn IO.stderr x


---  Hash options  ---

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

showHashFunction :: HashFunction -> String
showHashFunction = \case
    SHA_256 -> "sha256"

readHashFunctionText :: Strict.Text -> Maybe HashFunction
readHashFunctionText = Strict.Text.unpack >>> readHashFunctionString

readHashFunctionString :: String -> Maybe HashFunction
readHashFunctionString x = List.lookup x hashFunctions

hashFunctions :: [(String, HashFunction)]
hashFunctions = [(showHashFunction SHA_256, SHA_256)]

normalizeHashFunction :: String -> String
normalizeHashFunction =
    List.map Char.toLower . List.filter (\x -> not (List.elem x " -_"))
