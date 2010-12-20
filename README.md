* Cabal Dev
** Motivation

Performing consistent builds is critical in software development, but
the current system of per-user and per-system GHC package databases
interferes with this need for consistency.  It is difficult to
preciesly identify the dependencies of a given project, and changes
necessary to enable one project to build may render another project
inoperable.  If each project had a separate package database, each
project could be built in a sandbox.

** Usage

Cabal-dev is simple to use:

: $ cd <cabalized project dir>
: $ cabal-dev install

Cabal-dev will create a default sandbox named `cabal-dev` in the
current directory.  This will be populated with the project
dependencies, which are built and installed into a package database
within the sandbox.  The first cabal-dev build of a project typically
takes substantially longer than subsequent builds--don't worry, the
artifacts created will be re-used on subsequent builds unless you
remove the sandbox, or specify a different sandbox (with --sandbox=).

The project is then built, utilizing the sandboxed package database
rather than the user database.  (The GHC system database *is* still
used.  We recommend that only the core packages be installed to the
system package database to reduce the potential for conflicts.)

`cabal-dev install` uses cabal-install to issue build and installation
commands that place the project's build artifacts in the cabal-dev
sandbox, as well as leaving the binaries in the familiar `dist`
directory.

** Building with private dependencies

Cabal-dev also allows you to use un-released packages as though they
were on hackage with `cabal-dev add-source`.

For example, the `linux-ptrace` and `posix-waitpid` packages were only
recently uploaded to hackage.  Previously, cabal-dev was used to build
applications that depended on these two packages:

: $ ls
: linux-ptrace/  myProject/  posix-waitpid/
: $ cd myProject
: $ cabal-dev add-source ../linux-ptrace ../posix-waitpid
: $ cabal-dev install

Note that `cabal-dev add-source` accepts a list of source locations.

Be careful, however, because packages that have been added are not
tied to their original source locations any more.  Changes to the
`linux-ptrace` source in the above example will not be used by
`myProject` unless the user issues `cabal-dev add-source` with the
path to the `linux-ptrace` source again.  This is similar to the
`cabal install` step you may do now to enable a project to make use of
changes to a dependency.

There is currently one additional requirement when using `cabal-dev
add-source`.  The projects that are add-source'd must generate sdists
that will build.  Cabal-dev currently uses sdists to transport the
dependencies into the sandbox, so the project will not build if
critical files are left out of the sdist.  Note that the packages do
not need to sdist cleanly, most warnings are acceptable, so this is
rarely a problem.
