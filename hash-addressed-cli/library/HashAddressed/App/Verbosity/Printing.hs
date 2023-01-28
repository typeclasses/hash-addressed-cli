module HashAddressed.App.Verbosity.Printing
  (
    putNormalLn,
    putVerboseLn,
  )
  where

import Essentials
import HashAddressed.App.Verbosity.Type

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bool (not, (||))
import Prelude (String)

import qualified System.IO as IO

putNormalLn :: MonadIO m => Verbosity -> String -> m ()
putNormalLn Verbosity{ quiet } x =
    if quiet then pure () else liftIO $ IO.hPutStrLn IO.stdout x

putVerboseLn :: MonadIO m => Verbosity -> String -> m ()
putVerboseLn Verbosity{ quiet, verbose } x =
    if quiet || not verbose then pure () else liftIO $ IO.hPutStrLn IO.stderr x

