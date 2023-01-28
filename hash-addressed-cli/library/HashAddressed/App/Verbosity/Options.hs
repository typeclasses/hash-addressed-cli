module HashAddressed.App.Verbosity.Options
  (
    verboseOption, quietOption, verbosityOption,
  )
  where

import Essentials
import HashAddressed.App.Verbosity.Type

import qualified Options.Applicative as Options

verboseOption :: Options.Parser Bool
verboseOption = Options.switch $ Options.long "verbose" <> Options.help
    "Print miscellaneous commentary to the standard error stream"

quietOption :: Options.Parser Bool
quietOption = Options.switch $ Options.long "quiet" <> Options.help
    "Do not print normal output to the standard output stream"

verbosityOption :: Options.Parser Verbosity
verbosityOption = do
    quiet <- quietOption
    verbose <- verboseOption
    pure Verbosity{ quiet, verbose }
