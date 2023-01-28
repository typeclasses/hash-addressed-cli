module HashAddressed.App.Command.Type
  (
    Command,
  )
  where

import Prelude (IO, String)

import qualified Control.Monad.Trans.Except as Except
import qualified Options.Applicative as Options

type Command = Options.ParserInfo (Except.ExceptT String IO ())
