module Distribution.Dev.LocalRepo
    ( defaultLocalRepo
    , resolveLocalRepo
    , getLocalRepo
    , LocalRepository
    , localRepoPath
    )
where

import Data.Maybe ( listToMaybe )
import Distribution.Dev.Flags ( GlobalFlag(LocalRepo) )
import Distribution.Dev.Log ( debug )
import System.Directory ( canonicalizePath, createDirectoryIfMissing )

newtype LocalRepository = LocalRepository { localRepoPath :: FilePath }

defaultLocalRepo :: FilePath
defaultLocalRepo = "./cabal-dev/packages"

getLocalRepo :: [GlobalFlag] -> Maybe FilePath
getLocalRepo flgs = listToMaybe [ fn | LocalRepo fn <- flgs ]

resolveLocalRepo :: [GlobalFlag] -> IO LocalRepository
resolveLocalRepo flgs = do
  relLocalRepo <-
      case getLocalRepo flgs of
        Nothing -> do
          debug flgs $ "No local repository specified. Using " ++ defaultLocalRepo
          return defaultLocalRepo
        Just s -> return $ s

  localRepo <- canonicalizePath relLocalRepo
  debug flgs $ "Using " ++ localRepo ++ " as the local repository path"
  createDirectoryIfMissing True localRepo
  return $ LocalRepository localRepo
