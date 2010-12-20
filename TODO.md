# Bugs
 - GHC 7: `canonicalizePath` throws exceptions in recent versions of Directory, which is needed by ghc 7.  This causes `cabal-dev` to fail if the sandbox directory does not already exist.  One potential (but untested) workaround is to manually create the sandbox directory first, then run `cabal-dev`.
 - Sometimes rebuilds more than is necessary (possibly due to cabal-install behaviors).
 - Conflicts can occurr when packages are installed to the global package database.
 - short versions of command-line flags, particularly `-f<flagname>`, are somewhat buggy.
 - maintaining a sandbox for multiple related packages is still laborious (as is necessary when builing large, multi-package projects).

# Feature Ideas
 - support multiple unpacked packages
 - support a concept of projects: 
   - multiple packages with one shared sandbox
   - flags specified on a per-package basis
 - release a library interface to cabal-dev for custom project build scripts

   