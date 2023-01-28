module HashAddressed.App.Command.Type
  (
    Command,
    CommandAction,
  )
  where

import Prelude (IO, String)
import Data.Sequence (Seq)

import qualified Control.Monad.Trans.Except as Except
import qualified Options.Applicative as Options

type Command = Options.ParserInfo (CommandAction ())

type CommandAction a = Except.ExceptT (Seq String) IO a
