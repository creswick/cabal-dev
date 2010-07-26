{-# LANGUAGE CPP, GADTs, EmptyDataDecls #-}
module Distribution.Dev.Sandbox
    ( KnownVersion
    , PackageDbType(..)
    , Sandbox
    , UnknownVersion
    , cabalConf
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
import Data.Version              ( Version, showVersion )
import Distribution.Simple.Utils ( debug )
import Distribution.Verbosity    ( Verbosity )
import System.Directory          ( canonicalizePath, createDirectoryIfMissing
                                 , doesFileExist, copyFile )
import System.FilePath           ( (</>) )

#ifdef mingw32_HOST_OS
import System.IO ( hPutStrLn, stderr )
import System.Win32.Types ( getLastError )
#endif

import qualified Distribution.Dev.Flags as F
    ( getVerbosity, Config, getSandbox, sandboxSpecified )

import Paths_cabal_dev ( getDataFileName )

-- A sandbox directory that we may or may not know what kind of
-- package format it uses
data UnknownVersion
data KnownVersion

data Sandbox a where
    UnknownVersion :: FilePath -> Sandbox UnknownVersion
    KnownVersion :: FilePath -> PackageDbType -> Version -> Sandbox KnownVersion

data PackageDbType = GHC_6_8_Db FilePath | GHC_6_10_Db | GHC_6_12_Db

-- NOTE: GHC < 6.12: compilation warnings about non-exhaustive pattern
-- matches are spurious (we'd get a type error if we tried to make
-- them complete!)
setVersion :: Sandbox UnknownVersion -> PackageDbType -> Version -> Sandbox KnownVersion
setVersion (UnknownVersion p) v ty = KnownVersion p v ty

getVersion :: Sandbox KnownVersion -> PackageDbType
getVersion (KnownVersion _ db _) = db

sandbox :: Sandbox a -> FilePath
sandbox (UnknownVersion p) = p
sandbox (KnownVersion p _ _) = p

sPath :: FilePath -> Sandbox a -> FilePath
sPath p s = sandbox s </> p

localRepoPath :: Sandbox a -> FilePath
localRepoPath = sPath "packages"

pkgConf :: Sandbox KnownVersion -> FilePath
pkgConf s@(KnownVersion _ _ v) = sPath packageDbName s
    where
      packageDbName = "packages-" ++ showVersion v ++ ".conf"

cabalConf :: Sandbox a -> FilePath
cabalConf = sPath "cabal.config"

newSandbox :: Verbosity -> FilePath -> IO (Sandbox UnknownVersion)
newSandbox v relSandboxDir = do
  sandboxDir <- canonicalizePath relSandboxDir
  debug v $ "Using " ++ sandboxDir ++ " as the cabal-dev sandbox"
  vista32Workaround_createDirectoryIfMissing True sandboxDir
  let sb = UnknownVersion sandboxDir
  debug v $ "Creating local repo " ++ localRepoPath sb
  vista32Workaround_createDirectoryIfMissing True $ localRepoPath sb
  extant <- doesFileExist (indexTar sb)
  unless extant $ do
    emptyIdxFile <- getDataFileName $ "admin" </> indexTarBase
    copyFile emptyIdxFile (indexTar sb)
  return sb

-- | Ugly hack to try and get around a bug with
-- createDirectoryIfMissing that only occurs on 32-bit windows.
vista32Workaround_createDirectoryIfMissing :: Bool -> FilePath -> IO ()
vista32Workaround_createDirectoryIfMissing b fp =
#ifdef mingw32_HOST_OS
  createDirectoryIfMissing b fp `catch` \e -> do
    erCode <- getLastError
    case erCode of
      1006 -> hPutStrLn stderr "Directory already exists--error swallowed"
      _    -> ioError e
#else
  createDirectoryIfMissing b fp
#endif

resolveSandbox :: F.Config -> IO (Sandbox UnknownVersion)
resolveSandbox cfg = do
  let v = F.getVerbosity cfg
      relSandbox = F.getSandbox cfg
  unless (F.sandboxSpecified cfg) $
         debug v $ "No sandbox specified. Using " ++ relSandbox
  newSandbox v relSandbox

-- |The name of the cabal-install package index
indexTarBase :: FilePath
indexTarBase = "00-index.tar"

indexTar :: Sandbox a -> FilePath
indexTar sb = localRepoPath sb </> indexTarBase
