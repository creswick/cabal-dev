{-# LANGUAGE CPP #-}
module Distribution.Dev.InitPkgDb
    ( initPkgDb
    )
where

#ifndef MIN_VERSION_Cabal
#define MIN_VERSION_Cabal(x,y,z) 1
#endif

import Control.Monad ( unless )
import qualified Distribution.Verbosity as V
import Distribution.Version ( Version(..) )
import Distribution.Simple.Program ( ghcPkgProgram, requireProgram, programVersion, ConfiguredProgram )
#if MIN_VERSION_Cabal(1,8,0)
import Distribution.Simple.Program.Db ( emptyProgramDb )
import Distribution.Simple.Program ( runProgram )
#elif MIN_VERSION_Cabal(1,6,0)
import Distribution.Simple.Program ( emptyProgramConfiguration, rawSystemProgram )
import Distribution.Version ( VersionRange(AnyVersion) )
#else
#error Requires Cabal 1.6 or 1.8
#endif
import System.Directory ( doesFileExist, doesDirectoryExist )

import Distribution.Dev.LocalRepo ( Sandbox, pkgConf )

-- |Initialize a package database.
--
-- XXX: This is GHC-only. Perhaps we can take advantage of the Cabal
-- package database work.
--
-- XXX: If a compilation happens in a sandbox for that was used for a
-- GHC version with a different package config type, this function
-- will just fail. Ideally, we'd have different package DBs for
-- different GHC versions, but the cabal-install config file just sets
-- one location. We'd have to have the GHC version before writing the
-- cabal config file.
initPkgDb :: Sandbox -> IO ()
initPkgDb s = do
#if MIN_VERSION_Cabal(1,8,0)
  let require = requireProgram
      run     = runProgram
      empty   = emptyProgramDb
#elif MIN_VERSION_Cabal(1,6,0)
  let require = requireProgram anyVersion
      run     = rawSystemProgram
      empty   = emptyProgramConfiguration
#endif

  (ghcPkg, _) <- require V.normal ghcPkgProgram empty
  case ghcPackageDbType ghcPkg of
    FileDb -> do
      e <- doesFileExist (pkgConf s)
      unless e $ writeFile (pkgConf s) $ show ([] :: [()])
    DirDb  -> do
      e <- doesDirectoryExist (pkgConf s)
      unless e $ run V.normal ghcPkg ["init", pkgConf s]

data PackageDbType = FileDb | DirDb

ghcPackageDbType :: ConfiguredProgram -> PackageDbType
ghcPackageDbType p = case programVersion p of
                       Nothing -> error "Unknown ghc version!"
                       Just v | v < Version [6, 12] [] -> FileDb
                              | otherwise              -> DirDb
