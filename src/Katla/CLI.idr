module Katla.CLI

import public Collie

import Katla.Config

import Decidable.Decidable
import Data.List.Fresh
import Data.List.Fresh.Elem

import Decidable.Decidable.Extra1

%default covering

katlaCmd : Command "katla"
katlaCmd = MkCommand
  { description = """
      LaTeX code listing generator for Idris2
      """
  , subcommands =
    [ "--help"   ::= basic "Print this help text." none
    , "preamble" ::= preambleCmd
    ]
  , modifiers   = []
  , arguments = none
  }

infixr 4 -=->

0
(-=->) : (Command arg) -> Type -> Type
(-=->) cmd a = {0 covers : ParseTree Maybe Maybe cmd} ->
  {path : List String} ->
  {focus : _} -> {focusCmd : _} -> {parsed : ParsedCommand Maybe Maybe focus focusCmd } ->
  IsPathTo covers path parsed ->
  a


katlaExec : CLI.katlaCmd -=-> IO ()
katlaExec []           = putStrLn katlaCmd.usage
katlaExec ["--help"  ] = putStrLn katlaCmd.usage
katlaExec ["preamble"] = preambleExec parsed.arguments
                                      (parsed.modifiers.project "--config")
katlaExec ["preamble", "sub1"] = ?h1
katlaExec ["preamble", "sub2"] = ?h2

katlaExec {covers = %search} _ impossible
