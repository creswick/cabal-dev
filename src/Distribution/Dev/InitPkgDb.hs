{-# LANGUAGE CPP #-}
module Distribution.Dev.InitPkgDb
    ( initPkgDb
    )
where

import Control.Monad ( unless )
import qualified Distribution.Verbosity as V
import Distribution.Simple.Program ( ghcPkgProgram, requireProgram
                                   , programVersion, ConfiguredProgram
                                   , programLocation, locationPath
                                   )
import Distribution.Version ( Version(..) )


-- This makes MIN_VERSION_Cabal testing use the first option in the
-- absence of the macro (compilation without Cabal)
#ifndef MIN_VERSION_Cabal
#define MIN_VERSION_Cabal(a,b,c) 1
#endif

-- Select the appropriate imports for the version of Cabal
#if MIN_VERSION_Cabal(1,8,0)
import Distribution.Simple.Program ( runProgram )
import Distribution.Simple.Program.Db ( emptyProgramDb )
#elif MIN_VERSION_Cabal(1,2,0)
import Distribution.Simple.Program ( rawSystemProgram
                                   , emptyProgramConfiguration )
import Distribution.Version ( VersionRange(AnyVersion) )
#else
#error Cabal version not supported
#endif

import System.Directory ( doesFileExist, doesDirectoryExist )

import Distribution.Dev.Sandbox ( Sandbox, pkgConf, PackageDbType(..)
                                , UnknownVersion, KnownVersion, setVersion )

-- |Initialize a package database.
--
-- XXX: This is GHC-only. Cabal supports other compilers, so we should, too.
--
-- XXX: If a compilation happens in a sandbox for that was used for a
-- GHC version with a different package config type, this function
-- will just fail. Ideally, we'd have different package DBs for
-- different GHC versions, but the cabal-install config file just sets
-- one location. We'd have to have the GHC version before writing the
-- cabal config file.
initPkgDb :: Sandbox UnknownVersion -> IO (Sandbox KnownVersion)
initPkgDb s = do
#if MIN_VERSION_Cabal(1,8,0)
  let require v p = requireProgram v p emptyProgramDb
      run     = runProgram
#elif MIN_VERSION_Cabal(1,2,0)
  let require v p = requireProgram v p AnyVersion emptyProgramConfiguration
      run     = rawSystemProgram
#else
#error Cabal version not supported
#endif

  ghcPkg <- fst `fmap` require V.normal ghcPkgProgram
  let typ = ghcPackageDbType ghcPkg
      s' = setVersion s typ
      pth = pkgConf s'

  case typ of
    GHC_6_12_Db -> do
      e <- doesDirectoryExist pth
      unless e $ run V.normal ghcPkg ["init", pth]
    _ -> do
      e <- doesFileExist pth
      unless e $ writeFile pth "[]"
  return s'

ghcPackageDbType :: ConfiguredProgram -> PackageDbType
ghcPackageDbType p =
    case programVersion p of
      Nothing -> error "Unknown ghc version!"
      Just v | v < Version [6, 10] [] -> (GHC_6_8_Db $ locationPath $ programLocation p)
             | v < Version [6, 12] [] -> GHC_6_10_Db
             | otherwise              -> GHC_6_12_Db
