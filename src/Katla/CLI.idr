module Katla.CLI

import public Collie

import Katla.Config

%default covering

export
katlaCmd : Command "katla"
katlaCmd = MkCommand
  { description = """
      Katla v0.1.
      LaTeX code listing generator for Idris2
      """
  , subcommands =
    [ "--help"   ::= basic "Print this help text." none
    , "preamble" ::= preambleCmd
    ]
  , modifiers   = []
  , arguments = none
  }

export
katlaExec : CLI.katlaCmd -=-> IO ()
katlaExec []           = putStrLn katlaCmd.usage
katlaExec ["--help"  ] = putStrLn katlaCmd.usage
katlaExec ["preamble"] = preamble parsed
katlaExec {covers = %search} _ impossible
