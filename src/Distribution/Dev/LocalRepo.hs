module Distribution.Dev.LocalRepo
    ( defaultSandbox
    , resolveSandbox
    , getSandbox
    , Sandbox
    , localRepoPath
    , pkgConf
    , cabalConf
    )
where

import Data.Maybe ( listToMaybe )
import qualified Distribution.Dev.Flags as F ( GlobalFlag(Sandbox) )
import Distribution.Dev.Log ( debug )
import System.Directory ( canonicalizePath, createDirectoryIfMissing )
import System.FilePath ( (</>) )

newtype Sandbox = Sandbox FilePath

sPath :: FilePath -> Sandbox -> FilePath
sPath p (Sandbox s) = s </> p

localRepoPath :: Sandbox -> FilePath
localRepoPath = sPath "packages"

pkgConf :: Sandbox -> FilePath
pkgConf = sPath "packages.conf"

cabalConf :: Sandbox -> FilePath
cabalConf = sPath "cabal.config"

defaultSandbox :: FilePath
defaultSandbox = "./cabal-dev"

getSandbox :: [F.GlobalFlag] -> Maybe FilePath
getSandbox flgs = listToMaybe [ fn | F.Sandbox fn <- flgs ]

resolveSandbox :: [F.GlobalFlag] -> IO Sandbox
resolveSandbox flgs = do
  relSandbox <-
      case getSandbox flgs of
        Nothing -> do
          debug flgs $ "No local repository specified. Using " ++ defaultSandbox
          return defaultSandbox
        Just s -> return $ s

  localRepo <- canonicalizePath relSandbox
  debug flgs $ "Using " ++ localRepo ++ " as the local repository path"
  createDirectoryIfMissing True localRepo
  return $ Sandbox localRepo
