module Katla.Config

import Idrall.API.V2
import Language.Reflection
import Collie
import System.File
import System.Path
import Data.Maybe

%language ElabReflection


record Category where
  constructor MkCategory
  style : String
  colour : String

record Config where
  constructor MkConfig
  font : String
  datacons : Category
  typecons : Category
  bound    : Category
  function : Category
  keyword  : Category
  comment  : Category
  hole     : Category


defaultConfig : Config
defaultConfig = MkConfig
  { font = #"\ttfamily"#
  , datacons = MkCategory
    { style  = ""
    , colour = "IndianRed1"
    }
  , typecons = MkCategory
    { style  = ""
    , colour = "DeepSkyBlue3"
    }
  , bound = MkCategory
    { style  = ""
    , colour = "DarkOrchid3"
    }
  , function = MkCategory
    { style  = ""
    , colour = "Chartreuse4"
    }
  , keyword = MkCategory
    { style  = #"\bfseries"#
    , colour = "black"
    }
  , comment = MkCategory
    { style  = #"\itshape"#
    , colour = "grey"
    }
  , hole = MkCategory
    { style  = #"\bfseries"#
    , colour = "yellow"
    }
  }
%runElab (deriveFromDhall Record `{ Category})
%runElab (deriveFromDhall Record `{ Config})

||| Not yet used
export
laTeXHeader : Config -> String
laTeXHeader cfg =  """
\newcommand{\IdrisHlightFont}         {\{cfg.font}}
\newcommand{\IdrisHlightStyleData}    {\{cfg.datacons.style}}
\newcommand{\IdrisHlightStyleType}    {\{cfg.typecons.style}}
\newcommand{\IdrisHlightStyleBound}   {\{cfg.bound   .style}}
\newcommand{\IdrisHlightStyleFunction}{\{cfg.function.style}}
\newcommand{\IdrisHlightStyleKeyword} {\{cfg.keyword .style}}
\newcommand{\IdrisHlightStyleImplicit}{\{cfg.bound   .style}}
\newcommand{\IdrisHlightStyleComment} {\{cfg.comment .style}}
\newcommand{\IdrisHlightStyleHole}    {\{cfg.hole    .style}}

\newcommand{\IdrisHlightColourData}    {\{cfg.datacons.colour}}
\newcommand{\IdrisHlightColourType}    {\{cfg.typecons.colour}}
\newcommand{\IdrisHlightColourBound}   {\{cfg.bound   .colour}}
\newcommand{\IdrisHlightColourFunction}{\{cfg.function.colour}}
\newcommand{\IdrisHlightColourKeyword} {\{cfg.keyword .colour}}
\newcommand{\IdrisHlightColourImplicit}{\{cfg.bound   .colour}}
\newcommand{\IdrisHlightColourComment} {\{cfg.comment .colour}}
\newcommand{\IdrisHlightColourHole}    {\{cfg.hole    .colour}}

\newcommand{\IdrisHole}[1]{{%
    \colorbox{yellow}{%
      \IdrisHlightStyleHole\IdrisHlightFont%
      #1}}}

\newcommand{\RawIdrisHighlight}[3]{{\textcolor{#1}{#2\IdrisHlightFont#3}}}

\newcommand{\IdrisData}[1]{\RawIdrisHighlight{\IdrisHlightColourData}{\IdrisHlightStyleData}{#1}}
\newcommand{\IdrisType}[1]{\RawIdrisHighlight{\IdrisHlightColourType}{\IdrisHlightStyleType}{#1}}
\newcommand{\IdrisBound}[1]{\RawIdrisHighlight{\IdrisHlightColourBound}{\IdrisHlightStyleBound}{#1}}
\newcommand{\IdrisFunction}[1]{\RawIdrisHighlight{\IdrisHlightColourFunction}{\IdrisHlightStyleFunction}{#1}}
\newcommand{\IdrisKeyword}[1]{\RawIdrisHighlight{\IdrisHlightColourKeyword}{\IdrisHlightStyleKeyword}{#1}}
\newcommand{\IdrisImplicit}[1]{\RawIdrisHighlight{\IdrisHlightColourImplicit}{\IdrisHlightStyleImplicit}{#1}}
\newcommand{\IdrisComment}[1]{\RawIdrisHighlight{\IdrisHlightColourComment}{\IdrisHlightStyleComment}{#1}}
"""

public export
preambleCmd : Command "preamble"
preambleCmd = MkCommand
        { description = "Generate LaTeX preamble"
        , subcommands = [ "sub1" ::= basic "sub1 desc" none
                        , "sub2" ::= basic "sub2 desc" none]
        , modifiers =
          [ "--config" ::= option """
              Preamble configuration file in the following (Dhall) format:
              """ filePath
          ]
        , arguments = filePath
        }

export
getConfiguration : (configFile : Maybe String) -> IO Config
getConfiguration Nothing = pure defaultConfig
getConfiguration (Just filename) = do
  Right config <- liftIOEither (deriveFromDhallString {ty = Config} filename)
  | Left err => do putStrLn  """
                     Error while parsing configuration file \{filename}:
                     \{show err}
                     Using default configuration instead.
                     """
                   pure defaultConfig

  pure config

export
preambleExec : (moutput : Maybe String) -> (configFile : Maybe String) -> IO ()
preambleExec moutput configFile = do
  Right file <- maybe (pure $ Right stdout) (flip openFile WriteTruncate) moutput
  | Left err => putStrLn """
              Error while opening preamble file \{maybe "stdout" id moutput}:
              \{show err}
              """
  config <- getConfiguration configFile
  Right () <- fPutStr file $ laTeXHeader config
  | Left err => putStrLn """
      Error while writing preamble file \{maybe "stdout" id moutput}:
      \{show err}
      """
  closeFile file
