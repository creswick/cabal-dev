{- Copyright (c) 2010 Galois, Inc. -}
{-|

mk-repo command

Puts local source packages into a repository readable by cabal-install

-}
module Distribution.Dev.MkRepo
    ( actions
    )
where

import Control.Arrow ( right )
import Control.Applicative ( (<$>) )
import Control.Monad ( liftM, ap, guard, (<=<), unless, forM_ )
import Control.Exception ( bracket, catchJust )
import System.FilePath ( takeExtension, takeBaseName, splitDirectories, (<.>), (</>), splitExtension )
import System.Console.GetOpt ( OptDescr(..) )
import System.IO ( withFile, IOMode(..), openTempFile, hClose )
import System.Exit ( ExitCode(..) )
import System.IO.Error ( isDoesNotExistError )
import System.Directory ( getDirectoryContents, renameFile, copyFile, getCurrentDirectory, setCurrentDirectory, createDirectoryIfMissing )
import System.Cmd ( rawSystem )
import Distribution.Package ( PackageName(..), PackageIdentifier(..) )
import Distribution.PackageDescription.Parse ( readPackageDescription )
import Distribution.PackageDescription ( packageDescription, package )
import Distribution.Text ( simpleParse, display )
import qualified Data.ByteString.Lazy as L
import qualified Codec.Compression.GZip as Z
import qualified Codec.Archive.Tar as T
import qualified Codec.Archive.Tar.Entry as T
import qualified Distribution.Verbosity as V

import Distribution.Dev.Command ( CommandActions(..), CommandResult(..) )
import Distribution.Dev.Flags ( GlobalFlag )
import Distribution.Dev.LocalRepo ( resolveLocalRepo, localRepoPath, LocalRepository )
import qualified Distribution.Dev.Log as Log

actions :: CommandActions
actions = CommandActions
            { cmdDesc = "Add packages to a local cabal install repository"
            , cmdRun = \flgs _ -> mkRepo flgs
            , cmdOpts = [] :: [OptDescr ()]
            }

mkRepo :: [GlobalFlag] -> [String] -> IO CommandResult
mkRepo _    [] = return $ CommandError "No local package locations supplied"
mkRepo flgs fns = do
  localRepo <- resolveLocalRepo flgs
  Log.debug flgs $ "Making a cabal repo in " ++ localRepoPath localRepo ++
         " out of " ++ show fns
  (sources, newEntries) <- unzip `fmap` mapM processLocalSource fns
  existingIndex <- readExistingIndex localRepo
  let newIndex = mergeIndices existingIndex newEntries

  -- Now we have the new index ready and have sanity-checked all of
  -- the package locations to be sure that they contain a cabal
  -- package (or at least a .cabal file)
  --
  -- Now to install the tarballs for the directories:
  forM_ (zip sources fns) $ \((src, pkgId), fn) ->
      installTarball localRepo src pkgId fn

  -- and now that the tarballs are in place, write out the updated index
  writeIndex localRepo newIndex
  return CommandOk

-- |Atomically write an index tarball in the supplied directory
writeIndex :: LocalRepository -- ^The local repository path
           -> [T.Entry] -- ^The index entries
           -> IO ()
writeIndex localRepo ents =
    do newIndexName <- withTmpIndex $ \(fn, h) ->
                       L.hPut h (T.write ents) >> return fn
       renameFile newIndexName $ indexTar localRepo
    where
      pth = localRepoPath localRepo
      withTmpIndex = bracket (openTempFile pth indexTarBase) (hClose . snd)

-- |Merge two lists of tar entries, filtering out the entries from the
-- original list that will be duplicated by the second list of
-- entries
mergeIndices :: [T.Entry] -> [T.Entry] -> [T.Entry]
mergeIndices old new = filter notInNew old ++ new
    where
      newPaths = map T.entryTarPath new
      notInNew e = not $ T.entryTarPath e `elem` newPaths

-- |Create a tar entry for the package identifier and cabal file contents
toIndexEntry :: PackageIdentifier -> L.ByteString -> Either String T.Entry
toIndexEntry pkgId c = right toEnt $ T.toTarPath False (indexName pkgId)
    where
      toEnt p = T.fileEntry p c

-- |Read an existing index tarball from the local repository, if one
-- exists. If the file does not exist, behave as if the index has no
-- entries.
readExistingIndex :: LocalRepository -> IO [T.Entry]
readExistingIndex localRepo =
    catchJust (guard . isDoesNotExistError) readIndexFile (const $ return [])
    where
      readIndexFile = withFile (indexTar localRepo) ReadMode
                      (forceEntries . T.read <=< L.hGetContents)
      forceEntries es = do
        let es' = T.foldEntries (:) [] error es
        length es' `seq` return es'

-- |What kind of package source is this?
data LocalSource = DirPkg | TarPkg

-- |Determine if this filename looks like a tarball (otherwise, it
-- assumes that it's a directory and treats it as such)
classifyLocalSource :: FilePath -> LocalSource
classifyLocalSource fn | isTarball fn = TarPkg
                       | otherwise    = DirPkg

-- |Put the tarball for this package in the local repository
installTarball :: LocalRepository -- ^Location of the local repository
               -> LocalSource -- ^What kind of package source
               -> PackageIdentifier
               -> FilePath -- ^Where the package is in the filesystem
               -> IO ()
installTarball localRepo src pkgId fn =
    do createDirectoryIfMissing True $ localRepoPath localRepo </> repoDir pkgId
       case src of
         TarPkg -> copyFile fn dest
         DirPkg -> do
                  tarFn <- makeSDist
                  renameFile tarFn dest
    where
      dest = localRepoPath localRepo </> tarballName pkgId
      makeSDist =
          bracket getCurrentDirectory setCurrentDirectory $ \_ -> do
              setCurrentDirectory fn
              cabalRes <- rawSystem "cabal" ["sdist"]
              case cabalRes of
                ExitSuccess -> return ()
                ExitFailure code ->
                    error $ "cabal sdist failed with " ++ show code
              here <- getCurrentDirectory
              return $ here </> "dist" </> display pkgId <.> "tar" <.> "gz"

-- |Extract the index information from the supplied path, either as a
-- tarball or as a local package directory
processLocalSource :: FilePath
                   -> IO ((LocalSource, PackageIdentifier), T.Entry)
processLocalSource fn = do
  let src = classifyLocalSource fn
  (pkgId, c) <- case src of
           TarPkg -> processTarball fn
           DirPkg -> processDirectory fn
  ent <- either fail return $ toIndexEntry pkgId c
  return ((src, pkgId), ent)

-- |Extract the index information from a tarball
processTarball :: FilePath -> IO (PackageIdentifier, L.ByteString)
processTarball fn =
    withFile fn ReadMode $ \h ->
        do ents <- T.read . Z.decompress <$> L.hGetContents h
           case extractCabalFile ents of
             Nothing -> error "No cabal file found"

             -- Force reading the cabal file before we exit withFile
             Just res -> forceBS (snd res) >> return res

-- |Extract the index information from a directory containing a cabal
-- file
processDirectory :: FilePath -> IO (PackageIdentifier, L.ByteString)
processDirectory d = do
  fns <- getDirectoryContents d
  case filter isCabalFile fns of
    [] -> error "No cabal file found"
    [c] -> do
      let fn = d </> c
      pkgId <- package . packageDescription <$>
               readPackageDescription V.normal fn
      unless (PackageName (takeBaseName c) == pkgName pkgId) $
             error $ "Package name does not match\ 
                     \ cabal file name: " ++ fn
      cabalFile <- withFile fn ReadMode $ forcedBS <=< L.hGetContents
      return (pkgId, cabalFile)
    _ -> error "More than one cabal file present"

-- |Force a lazy ByteString to be read
forceBS :: L.ByteString -> IO ()
forceBS bs = L.length bs `seq` return ()

-- |Force a lazy ByteString to be read, and pass it on to the next action
forcedBS :: L.ByteString -> IO L.ByteString
forcedBS bs = forceBS bs >> return bs

-- |Extract a cabal file from a package tarball
extractCabalFile :: T.Entries -> Maybe (PackageIdentifier, L.ByteString)
extractCabalFile = T.foldEntries step Nothing (const Nothing)
    where
      step ent Nothing = (,) `liftM` entPackageId ent `ap` entBytes ent
      step _   ans     = ans

      entPackageId ent =
          case splitDirectories $ T.entryPath ent of
            [d, f] ->
                do i <- simpleParse d
                   let cabalName = PackageName $ takeBaseName f
                   guard $ isCabalFile f && cabalName == pkgName i
                   return i
            _      -> Nothing

      entBytes ent = case T.entryContent ent of
                       T.NormalFile x _ -> return x
                       _ -> Nothing

-- | Does this filename look like a cabal file?
isCabalFile :: FilePath -> Bool
isCabalFile = (== ".cabal") . takeExtension

-- |Does this filename look like a gzipped tarball?
isTarball :: FilePath -> Bool
isTarball fn = (ext2, ext1) == (".tar", ".gz")
    where
      (fn1, ext1) = splitExtension fn
      (_, ext2) = splitExtension fn1

-- |The path to the .cabal file in the 00-index.tar file
indexName :: PackageIdentifier -> FilePath
indexName pkgId = repoDir pkgId </> (display (pkgName pkgId) <.> "cabal")

-- |The path to the tarball in the local repository
tarballName :: PackageIdentifier -> FilePath
tarballName pkgId = repoDir pkgId </> (display pkgId <.> "tar" <.> "gz")

repoDir :: PackageIdentifier -> FilePath
repoDir pkgId = display (pkgName pkgId) </>
                display (pkgVersion pkgId)

-- |The name of the cabal-install package index
indexTarBase :: FilePath
indexTarBase = "00-index.tar"

indexTar :: LocalRepository -> FilePath
indexTar lr = localRepoPath lr </> indexTarBase
