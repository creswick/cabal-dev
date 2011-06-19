
# What do I do about "cabal: internal error: unexpected package db stack" ?

    $ cabal-dev configure
    Resolving dependencies...
    cabal: internal error: unexpected package db stack

This is caused when you 'cabal-dev configure' that fails with/due to (?) a side effect.

Removing the 'dist' directory (and issuing a successfull configure, if needed) should get past this problem.

We have generally found that `cabal-dev configure` is unnecessary.
`cabal-dev install` is almost always sufficient to collect
dependencies, configure, build, and install (in a sandbox).
`cabal-dev configure` usually just adds confusion.

If you find a situation that *needs* `cabal-dev configure` please let
us know.