{-# LANGUAGE CPP, GADTs, EmptyDataDecls #-}
module Distribution.Dev.Sandbox
    ( KnownVersion
    , PackageDbType(..)
    , Sandbox
    , UnknownVersion
    , cabalConf
    , defaultSandbox
    , getSandbox
    , getVersion
    , indexTar
    , indexTarBase
    , localRepoPath
    , newSandbox
    , pkgConf
    , resolveSandbox
    , sandbox
    , setVersion
    )
where

import Control.Monad             ( unless )
import Data.Maybe                ( listToMaybe )
import Distribution.Simple.Utils ( debug )
import Distribution.Verbosity    ( Verbosity )
import System.Directory          ( canonicalizePath, createDirectoryIfMissing
                                 , doesFileExist, copyFile )
import System.FilePath           ( (</>) )
import System.IO.Error           ( try, isAlreadyExistsError, IOError )

import qualified Distribution.Dev.Flags as F ( GlobalFlag(Sandbox), getVerbosity )

import Paths_cabal_dev ( getDataFileName )

-- A sandbox directory that we may or may not know what kind of
-- package format it uses
data UnknownVersion
data KnownVersion

data Sandbox a where
    UnknownVersion :: FilePath -> Sandbox UnknownVersion
    KnownVersion :: FilePath -> PackageDbType -> Sandbox KnownVersion

data PackageDbType = GHC_6_8_Db FilePath | GHC_6_10_Db | GHC_6_12_Db

-- NOTE: GHC < 6.12: compilation warnings about non-exhaustive pattern
-- matches are spurious (we'd get a type error if we tried to make
-- them complete!)
setVersion :: Sandbox UnknownVersion -> PackageDbType -> Sandbox KnownVersion
setVersion (UnknownVersion p) ty = KnownVersion p ty

getVersion :: Sandbox KnownVersion -> PackageDbType
getVersion (KnownVersion _ db) = db

sandbox :: Sandbox a -> FilePath
sandbox (UnknownVersion p) = p
sandbox (KnownVersion p _) = p

sPath :: FilePath -> Sandbox a -> FilePath
sPath p s = sandbox s </> p

localRepoPath :: Sandbox a -> FilePath
localRepoPath = sPath "packages"

pkgConf :: Sandbox KnownVersion -> FilePath
pkgConf s@(KnownVersion _ ty) = sPath (packageDbName ty) s
    where
      packageDbName (GHC_6_8_Db _) = "packages-6.8.conf"
      packageDbName GHC_6_10_Db = "packages-6.10.conf"
      packageDbName GHC_6_12_Db = "packages.conf.d"

cabalConf :: Sandbox a -> FilePath
cabalConf = sPath "cabal.config"

defaultSandbox :: FilePath
defaultSandbox = "./cabal-dev"

getSandbox :: [F.GlobalFlag] -> Maybe FilePath
getSandbox flgs = listToMaybe [ fn | F.Sandbox fn <- flgs ]

newSandbox :: Verbosity -> FilePath -> IO (Sandbox UnknownVersion)
newSandbox v relSandboxDir = do
  sandboxDir <- canonicalizePath relSandboxDir
  debug v $ "Using " ++ sandboxDir ++ " as the cabal-dev sandbox"
  robust_createDirectoryIfMissing True sandboxDir
  let sb = UnknownVersion sandboxDir
  debug v $ "Creating local repo " ++ localRepoPath sb
  robust_createDirectoryIfMissing True $ localRepoPath sb
  extant <- doesFileExist (indexTar sb)
  unless extant $ do
    emptyIdxFile <- getDataFileName $ "admin" </> indexTarBase
    copyFile emptyIdxFile (indexTar sb)
  return sb

-- | Ugly hack to try and get around a bug with
-- createDirectoryIfMissing that only occurs on 32-bit windows.
robust_createDirectoryIfMissing :: Bool -> FilePath -> IO ()
robust_createDirectoryIfMissing b fp =
#ifdef mingw32_HOST_OS
  do
    r <- try $ createDirectoryIfMissing b fp
    case (r :: Either IOException ()) of
      Right ()                    -> return ()
      Left e
        | isAlreadyExistsError e -> return ()
        | otherwise              -> throw e
#else
  createDirectoryIfMissing b fp
#endif

resolveSandbox :: [F.GlobalFlag] -> IO (Sandbox UnknownVersion)
resolveSandbox flgs = do
  let v = F.getVerbosity flgs
  relSandbox <-
      case getSandbox flgs of
        Nothing -> do
          debug v $ "No sandbox specified. Using " ++ defaultSandbox
          return defaultSandbox
        Just s -> return $ s

  newSandbox v relSandbox

-- |The name of the cabal-install package index
indexTarBase :: FilePath
indexTarBase = "00-index.tar"

indexTar :: Sandbox a -> FilePath
indexTar sb = localRepoPath sb </> indexTarBase
