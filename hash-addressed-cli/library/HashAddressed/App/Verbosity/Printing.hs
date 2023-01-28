module HashAddressed.App.Verbosity.Printing
  (
    putNormalLn,
    putVerboseLn,
  )
  where

import Essentials
import HashAddressed.App.Verbosity.Type

import Control.Monad.IO.Class (MonadIO, liftIO)
import Prelude (String)

import qualified Control.Monad as Monad
import qualified System.IO as IO

{-| For output that one might want to use programmatically

    Goes to stdout.

    Can be suppressed with the @--quiet@ flag. -}
putNormalLn :: MonadIO m => Verbosity -> String -> m ()
putNormalLn Verbosity{ quiet } x =
    Monad.unless quiet $ liftIO $ IO.hPutStrLn IO.stdout x

{-| For extra chatty messages

    Goes to stderr so that the output that one might want to use
    programmatically can be captured separately.

    Does not print unless the @--verbose@ flag is used. -}
putVerboseLn :: MonadIO m => Verbosity -> String -> m ()
putVerboseLn Verbosity{ verbose } x =
    Monad.when verbose $ liftIO $ IO.hPutStrLn IO.stderr x
