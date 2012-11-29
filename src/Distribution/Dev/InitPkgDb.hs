{-# LANGUAGE CPP #-}
module Distribution.Dev.InitPkgDb
    ( initPkgDb
    )
where


import Control.Monad ( unless )
import Distribution.Simple.Program ( ghcPkgProgram, requireProgram
                                   , programVersion, ConfiguredProgram
                                   , programLocation, locationPath
                                   )
import Distribution.Version ( Version(..) )
import Distribution.Verbosity ( Verbosity )
import Distribution.Simple.Utils ( info )

import Distribution.Simple.Program ( rawSystemProgram, emptyProgramConfiguration )
import Distribution.Text ( display )

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
initPkgDb :: Verbosity -> Sandbox UnknownVersion -> IO (Sandbox KnownVersion)
initPkgDb v s = do
  let require p = requireProgram v p emptyProgramConfiguration
      run     = rawSystemProgram

  ghcPkg <- fst `fmap` require ghcPkgProgram
  let (ver, typ) = ghcPackageDbType ghcPkg
      s' = setVersion s typ ver
      pth = pkgConf s'

  if typ >= GHC_6_12_Db
    then do
      e <- doesDirectoryExist pth
      unless e $ run v ghcPkg ["init", pth]
    else do
      e <- doesFileExist pth
      unless e $ writeFile pth "[]"

  info v $ "Using ghc-pkg " ++ display ver ++
       case typ of
         GHC_6_8_Db _ -> " wrapper"
         _ -> ""

  return s'

ghcPackageDbType :: ConfiguredProgram -> (Version, PackageDbType)
ghcPackageDbType p =
    case res of
      Nothing -> error "Unknown ghc version!"
      Just v  -> v
    where
      res = do
        v <- programVersion p
        -- XXX Needs updated for ghc 7
        let typ | v < Version [6, 10] [] = GHC_6_8_Db $ locationPath $
                                           programLocation p
                | v < Version [6, 12] [] = GHC_6_10_Db
                | v >= Version [7, 5] [] = GHC_7_5_Plus_Db
                | otherwise              = GHC_6_12_Db
        return (v, typ)
