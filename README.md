# Cabal Dev
## Motivation

Performing consistent builds is critical in software development, but
the current system of per-user and per-system GHC package databases
interferes with this need for consistency.  It is difficult to
preciesly identify the dependencies of a given project, and changes
necessary to enable one project to build may render another project
inoperable.  If each project had a separate package database, each
project could be built in a sandbox.

## Usage

Cabal-dev is simple to use:

    $ cd <cabalized project dir>
    $ cabal-dev install

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

If you are developing multiple interdependent packages together, see
the section below about building with private dependencies.

See cabal-dev --help for detailed usage information.

### Ghci with cabal-dev

Cabal-dev 0.7.3.1 and greater are capable of launching ghci with the
project's package database and local modules (if the package under
development exposes a library).

    # First, you must cabal-dev install the package to populate the
    # package database:
    $ cabal-dev install
    ....
    <snip>
    ....
    $ cabal-dev ghci
    GHCi, version 6.12.3: http://www.haskell.org/ghc/  :? for help
    Loading package ghc-prim ... linking ... done.
    Loading package integer-gmp ... linking ... done.
    Loading package base ... linking ... done.
    Loading package ffi-1.0 ... linking ... done.
    Prelude>

The ghci shell should have access to all the libraries your
application/library is using, as well as any modules that your library
exposes.

Note that this is not quite as natural as your traditional ghci shell,
namely: Source modifications are not visible without exiting,
re-issuing `cabal-dev install` *and* `cabal-dev ghci`.  This will
eventually get better, but that's where things are right now.  The
reason for this is that `cabal-dev ghci` just issues ghci with the
cabal-dev package database (and excluding the user package db, to best
reflect what cabal-dev does when it causes compilation).

## Building with private dependencies

Cabal-dev supports two different workflows for using un-released
packages. The first makes the dependencies available in a
sandbox-local Hackage. The second rebuilds the dependent package every
time. If any of your provate dependencies are unchanging (e.g. you had
to patch a dependency to relax a package constraint) you will probably
want to use add-source. If you are actively developing two packages
that have dependencies on each other, you probably will prefer the
second. You can mix and match these techniques seamlessly.

### Using a sandbox-local Hackage

Cabal-dev also allows you to use un-released packages as though they
were on hackage with `cabal-dev add-source`.

For example, the `linux-ptrace` and `posix-waitpid` packages were only
recently uploaded to hackage.  Previously, cabal-dev was used to build
applications that depended on these two packages:

    $ ls
    linux-ptrace/  myProject/  posix-waitpid/
    $ cd myProject
    $ cabal-dev add-source ../linux-ptrace ../posix-waitpid
    $ cabal-dev install

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

### Building multiple packages together

For packages that are being actively developed together, recent
cabal-install (> 0.10) provides another option: specify all of the
source directories together on the cabal-install command line. For
example, say we're developing a Web application that depends on a
helper package that we use for other Web projects as well. We keep
making changes to both the application and its helper package. To
build both packages together:

 $ ls
 my-webapp/  webapp-helpers/
 $ cabal-dev install my-webapp/ webapp-helpers/

Note that this is a feature of newer cabal-install and is not limited
to use with cabal-dev, but cabal-dev makes it more useful by keeping
the development code isolated from other builds.

The disadvantage of this approach is that it can be slow. When you
have not made changes to a dependency, cabal-install will re-link and
reinstall it anyway (although it does avoid recompilation when it's
not necessary). Use the add-source mechanism if you have a dependency
that changes very infrequently.
