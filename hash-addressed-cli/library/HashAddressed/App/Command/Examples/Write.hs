module HashAddressed.App.Command.Examples.Write
  (
    writeCommand,
  )
  where

import Essentials
import HashAddressed.App.Command.Type
import HashAddressed.App.HashFunction.Naming
import HashAddressed.App.HashFunction.Options
import HashAddressed.App.Meta.Initialization
import HashAddressed.App.Meta.Paths
import HashAddressed.App.Meta.Reading
import HashAddressed.App.Verbosity.Options
import HashAddressed.App.Verbosity.Printing
import HashAddressed.App.Verbosity.Type
import HashAddressed.HashFunction

import Control.Monad.IO.Class (liftIO)
import HashAddressed.Directory (WriteResult (..), WriteType (..))
import Prelude (FilePath, IO)
import Data.Foldable (fold)

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.ByteString as Strict
import qualified Data.ByteString as Strict.ByteString
import qualified HashAddressed.Directory
import qualified Options.Applicative as Options
import qualified System.IO as IO
import qualified Data.Sequence as Seq
import qualified Control.Exception.Safe as Exception
import qualified Data.Either as Either
import qualified System.Directory as Directory

writeCommand :: Command
writeCommand = Options.info (parser <**> Options.helper) $ Options.progDesc
    "Copy from the standard input stream (or a file, see --source-file) \
    \to a hash-addressed store (see --target-directory)"
  where
    parser :: Options.Parser (CommandAction ())
    parser = do
        optStoreDirectory :: FilePath <-
            Options.strOption $ Options.long "target-directory" <>
                Options.help "Where the hash-addressed files are located"

        optSourceFile :: Maybe FilePath <-
            Options.optional $ Options.strOption $ Options.long "source-file" <>
                Options.help "Path of file to copy to the store; if this option is \
                    \not given, will read from standard input stream instead"

        optLinks :: [FilePath] <-
            Options.many $ Options.strOption $ Options.long "link" <>
                Options.help "After writing, create a symbolic link at this path \
                    \that points to the hash-addressed file. \
                    \This option may be given more than once to create multiple links. \
                    \The destination path path must be empty and its parent directory \
                    \must already exist. The process returns a non-zero exit code if \
                    \any of the links cannot be created."

        optInitializeStore :: Bool <-
            Options.switch $ Options.long "initialize" <>
                Options.help "Set up a hash-addressed store if one does not already exist. \
                \If this option is given, --hash-function is required."

        optHashFunction :: Maybe HashFunction <- Options.optional $
            Options.option hashFunctionRead $ Options.long "hash-function" <>
                Options.help ("If --initialize is given, use this flag to specify the hash \
                    \function. If a store exists, fail unless it used this hash function. "
                    <> hashFunctionInstructions)

        optVerbosity :: Verbosity <- verbosityOption

        pure do
            hashFunction <-
                case optInitializeStore of
                    True -> case optHashFunction of
                        Nothing -> Except.throwE $ Seq.singleton $ "--initialize requires --hash-function"
                        Just hf -> do
                            Monad.when optInitializeStore $ tryInitializeStore CreateIfNotPresent
                                optVerbosity hf optStoreDirectory
                            pure hf
                    False -> do
                        configHashFunction <- readHashFunctionFromConfig optStoreDirectory
                        case optHashFunction of
                            Just hf | hf /= configHashFunction -> Except.throwE $ Seq.singleton $
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

                liftIO $ HashAddressed.Directory.writeStreaming store
                    \(writeChunk :: Strict.ByteString -> m ()) -> do
                        let
                          loop :: m ()
                          loop = do
                            x <- liftIO $ Strict.ByteString.hGetSome input 4096
                            case Strict.ByteString.null x of
                                False -> writeChunk x *> loop
                                True -> pure ()
                        loop

            putNormalLn optVerbosity contentAddressedFile

            putVerboseLn optVerbosity case writeType of
                AlreadyPresent -> "The file was already present in the store; no change was made."
                NewContent -> "One new file was added to the store."

            linkFailures <- fmap fold $ liftIO $ optLinks & traverse \linkToBeCreated ->
                Exception.tryIO (Directory.createFileLink contentAddressedFile linkToBeCreated) <&> \case
                    Either.Left _ -> Seq.singleton $ "Failed to create link " <> linkToBeCreated
                    Either.Right () -> Seq.empty

            Monad.unless (Seq.null linkFailures) $ Except.throwE linkFailures
