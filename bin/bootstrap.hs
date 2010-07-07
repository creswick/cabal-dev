-- Bootstrap cabal-dev in a sandbox (not installing any dependencies in the
-- user or global package databases)
--
-- This script runs on GHC 6.8 - GHC 6.12.
--
-- Run in the root of this package with:
-- @
--  runhaskell -isrc bin/runtests.hs
-- @
import qualified Distribution.Dev.RewriteCabalConfig as R

import Data.List ( isPrefixOf )
import Control.Monad ( unless )
import Distribution.Simple.Utils ( rawSystemExit, rawSystemStdout )
import Distribution.Verbosity ( Verbosity, normal, verbose, showForCabal )
import System.Directory ( getHomeDirectory, canonicalizePath, doesFileExist
                        , doesDirectoryExist, getCurrentDirectory
                        , createDirectoryIfMissing )
import System.FilePath ( (</>) )
import System.Environment ( getArgs )

-- We would use cabal-dev itself here, but we use something a little
-- more brittle in order to make this code work with the Cabal that
-- came with GHC >= 6.8 so that we can bootstrap an installation
-- environment without installing any dependencies in a location that
-- could pollute other builds. RewriteCabalConfig is carefully written
-- so that it will work out-of-the-box with GHC >= 6.8 && < 6.13

-- TODO:
--  * Attempt to use cabal-dev if it's present at the start
--  * Invoke the test executable

main :: IO ()
main = do
  args <- getArgs
  let vb = case args of
             [] -> normal
             ["-v"] -> verbose
             ["--verbose"] -> verbose
             _ -> error $ "Unrecognized arguments: " ++ show args

  -- Create a sandbox to hold the installation
  sandbox <- getSandbox
  createDirectoryIfMissing True sandbox

  -- Bootstrap the ghc-pkg database for any dependencies we need to
  -- install in the process of building cabal-dev
  (ghcPkgDb, ghcVer) <- initGhcPkgDb vb sandbox

  -- Build a config file that points to the sandbox installation
  -- locations
  cfgOut <- createCabalConfig sandbox ghcPkgDb

  -- Invoke cabal-install running in this sandbox
  cabalArgs <- getCabalArgs vb sandbox cfgOut ghcVer
  rawSystemExit vb "cabal" $
                cabalArgs ++ [ "--config-file=" ++ cfgOut
                             , "install"
                             , "--flags=build-tests"
                             , "--verbose=" ++ showForCabal vb
                             ]
  let bin = (sandbox </>) . ("bin" </>)
  rawSystemExit vb (bin "cabal-dev-test") [bin "cabal-dev"]

-- The absolute path to the sandbox directory
getSandbox :: IO FilePath
getSandbox = canonicalizePath "cabal-dev"

---------------------------------------------------------------------
-- Identifying GHC version so that we know how to initialize and what
-- flags we need to supply to use the sandbox

-- We would ideally use Distribution.Dev.InitPkgDb here, but the Cabal
-- APIs that we are using changed between GHC releases, and since
-- we're not running under cabal (yet), it's not possible to use e.g.
-- CPP hacks to conditionally use them. We might want to introspect
-- the library to see how to call it as part of the build process, but
-- I expect that would be more work than re-implementing the minimal
-- functionality we need here.

data GHCVer = V6_8 | V6_10 | V6_12

-- |Initialize a GHC package database in the specified sandbox,
-- returning the package database path and the version of GHC that it
-- corresponds to.
initGhcPkgDb :: Verbosity -> FilePath -> IO (FilePath, GHCVer)
initGhcPkgDb vb sbox = do
  ver <- identifyGhcPkg vb
  let (fn, doInit) = ghcPkgSettings ver
      loc = sbox </> fn

  -- Actually initialize the database
  doInit vb loc

  return (loc, ver)

-- |Identify the GHC version by calling ghc-pkg and parsing its output
identifyGhcPkg :: Verbosity -> IO GHCVer
identifyGhcPkg vb = do
  verStr <- rawSystemStdout vb "ghc-pkg" ["--version"]
  let isVer6 v = isGHCPkg6 v verStr
  case () of
    _ | isVer6 8  -> return V6_8
      | isVer6 10 -> return V6_10
      | isVer6 12 -> return V6_12
      | otherwise -> fail $ "Failed to identify GHC from: " ++ show verStr

-- |Generate a ghc-pkg 6 version string prefix
ghcPkg6Ident :: Int -> String
ghcPkg6Ident v = "GHC package manager version 6." ++ show v ++ "."

-- |Does the provided string look like a ghc-pkg version string?
isGHCPkg6 :: Int -> String -> Bool
isGHCPkg6 v = (ghcPkg6Ident v `isPrefixOf`)

-- |Get the ghc-pkg filename and the appropriate procedure for
-- initializing a ghc package database for this version
--
-- XXX: this is duplicated in Distribution.Dev.InitPkgDb
ghcPkgSettings :: GHCVer -> (FilePath, Verbosity -> FilePath -> IO ())
ghcPkgSettings V6_8 = ("packages-6.8.conf", fileBased)
ghcPkgSettings V6_10 = ("packages-6.10.conf", fileBased)
ghcPkgSettings V6_12 = ("packages.conf.d", ghcPkgInit)

-- |Call ghc-pkg init (GHC >= 6.12)
ghcPkgInit :: Verbosity -> FilePath -> IO ()
ghcPkgInit vb pkgLoc = do
  extant <- doesDirectoryExist pkgLoc
  unless extant $ rawSystemExit vb "ghc-pkg" ["init", pkgLoc]

-- |Write an empty GHC package database for GHC >= 6.8 && < 6.12
fileBased :: Verbosity -> FilePath -> IO ()
fileBased _ pkgLoc = do
  extant <- doesFileExist pkgLoc
  unless extant $ writeFile pkgLoc "[]"

---------------------------------------------------------------
-- Invoking cabal with the correct arguments to use the sandbox

-- |Get the command-line arguments that we need to supply to
-- cabal-install in order to use the sandbox to download packages and
-- install depenencies
getCabalArgs :: Verbosity -> FilePath -> FilePath -> GHCVer -> IO [String]
getCabalArgs vb sandbox cfgFile v = (cfgFileArg:) `fmap` extraArgs
    where
      cfgFileArg = "--config-file=" ++ cfgFile
      extraArgs = case v of
                    V6_8 -> v68Args
                    _    -> return []

      v68Args = do
        -- Build ghc-pkg-6_8-compat by itself. It has no build
        -- dependencies aside from base and Cabal > 1.2, so we can
        -- build it without installing any dependencies anywhere.
        rawSystemExit vb "cabal"
                          [ "--config-file=" ++ cfgFile
                          , "install", "--flags=no-cabal-dev" ]

        -- Now we can use ghc-pkg-6_8_compat when installing
        -- dependencies for the rest of cabal-dev (notably, a newer
        -- version of Cabal)
        let compat = sandbox </> "bin" </> "ghc-pkg-6_8-compat"

        -- We don't tell ghc-pkg-6_8-compat where the original ghc-pkg
        -- is. Just use the version on the PATH because that's how we
        -- found it in the first place.
        return [ "--with-ghc-pkg=" ++ compat ]

-- Rewrite the cabal-install config file to have absolute paths
createCabalConfig :: FilePath -> FilePath -> IO FilePath
createCabalConfig sandbox ghcPkgDb = do
  let cfgIn = "admin" </> "cabal-config.in"
  let cfgOut = sandbox </> "cabal.config"
  home <- getHomeDirectory
  either fail (writeFile cfgOut)
      =<< R.rewriteCabalConfig (R.Rewrite home sandbox ghcPkgDb)
      =<< readFile cfgIn
  return cfgOut

