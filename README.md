# Katla: LaTeX code listing generator for Idris2

Dependencies:
+ Idris2 downstream of [PR#1335](https://github.com/idris-lang/Idris2/pull/1335)
+ [Installing](https://github.com/idris-lang/Idris2/blob/master/INSTALL.md#6-optional-installing-the-idris-2-api) the Idris 2 API
+ [Collie](https://github.com/ohad/collie) library for command line interfaces
+ [Idrall](https://github.com/alexhumphreys/idrall) library for
  [Dhall](https://dhall-lang.org/) bindings

# Demo
`make temp/Example.pdf`

# PRs welcome!

Wishlist:

+ Error handling
+ Better support for incorporating into bigger documents by generating macros
+ Batch mode
+ Separate into a library + driver module (so we could write Idris programs that generate such sources)
