module HashAddressed.App.Command.Examples.Write where

import Essentials
import HashAddressed.App.Command.Type
import HashAddressed.App.Verbosity.Printing
import HashAddressed.App.Verbosity.Options
import HashAddressed.App.HashFunction.Options
import HashAddressed.App.HashFunction.Naming
import HashAddressed.App.Meta.Initialization
import HashAddressed.App.Meta.Reading
import HashAddressed.App.Meta.Paths

import Control.Monad.IO.Class (liftIO)
import HashAddressed.Directory (WriteResult (..), WriteType (..))
import Prelude (IO)

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.ByteString as Strict
import qualified Data.ByteString as Strict.ByteString
import qualified HashAddressed.Directory
import qualified Options.Applicative as Options
import qualified System.IO as IO

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
