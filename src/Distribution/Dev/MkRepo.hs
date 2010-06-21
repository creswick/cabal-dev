{- Copyright (c) 2010 Galois, Inc. -}
{-|

mk-repo command

Puts local source packages into a repository readable by cabal-install

-}
module Distribution.Dev.MkRepo
    ( actions
    )
where

import Control.Applicative                   ( (<$>), (<*>) )
import Control.Arrow                         ( right )
import Control.Exception                     ( bracket, catchJust )
import Control.Monad                         ( guard, (<=<), forM_ )
import Distribution.Package                  ( PackageName(..)
                                             , PackageIdentifier(..)
                                             )
import Distribution.PackageDescription       ( packageDescription, package )
import Distribution.PackageDescription.Parse ( readPackageDescription )
import Distribution.Text                     ( simpleParse, display )
import System.Cmd                            ( rawSystem )
import System.Console.GetOpt                 ( OptDescr(..) )
import System.Directory                      ( getDirectoryContents
                                             , renameFile, copyFile
                                             , getCurrentDirectory
                                             , setCurrentDirectory
                                             , createDirectoryIfMissing
                                             )
import System.Exit                           ( ExitCode(..) )
import System.FilePath                       ( takeExtension, takeBaseName
                                             , splitDirectories, (<.>), (</>)
                                             , splitExtension
                                             )
import System.IO                             ( withFile, IOMode(..), hClose
                                             , openTempFile
                                             )
import System.IO.Error                       ( isDoesNotExistError )

import qualified Codec.Archive.Tar       as T
import qualified Codec.Archive.Tar.Entry as T
import qualified Codec.Compression.GZip  as Z
import qualified Data.ByteString.Lazy    as L
import qualified Distribution.Verbosity  as V

import Distribution.Dev.Command   ( CommandActions(..), CommandResult(..) )
import Distribution.Dev.Flags     ( GlobalFlag )
import Distribution.Dev.LocalRepo ( resolveLocalRepo, localRepoPath
                                  , LocalRepository
                                  )

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
  results <- mapM processLocalSource fns
  let errs = [e | Left e <- results]
      srcs = [s | Right s <- results]
  if not $ null errs
    then return $ CommandError $ unlines $
             "Errors finding cabal files:":map (showString "  ") errs
    else do
      let (sources, newEntries) = unzip srcs
      res <- readExistingIndex localRepo
      case res of
        Left err -> return $
                    CommandError $ "Error reading existing index: " ++ err
        Right existingIndex ->
            do let newIndex = mergeIndices existingIndex newEntries
               -- Now we have the new index ready and have sanity-checked
               -- all of the package locations to be sure that they
               -- contain a cabal package (or at least a .cabal file)
               --
               -- Now to install the tarballs for the directories:
               forM_ (zip sources fns) $ \((src, pkgId), fn) ->
                   installTarball flgs localRepo src pkgId fn

               -- and now that the tarballs are in place, write out the
               -- updated index
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
readExistingIndex :: LocalRepository -> IO (Either String [T.Entry])
readExistingIndex localRepo =
    catchJust (guard . isDoesNotExistError) readIndexFile $ \() ->
        return $ Right []
    where
      readIndexFile = withFile (indexTar localRepo) ReadMode
                      (forceEntries . T.read <=< L.hGetContents)
      forceEntries es =
        let step _ l@(Left _) = l
            step x (Right xs) = Right (x:xs)
            es' = T.foldEntries step (Right []) Left es
        in either (const 0) length es' `seq` return es'


-- |What kind of package source is this?
data LocalSource = DirPkg | TarPkg

-- |Determine if this filename looks like a tarball (otherwise, it
-- assumes that it's a directory and treats it as such)
classifyLocalSource :: FilePath -> LocalSource
classifyLocalSource fn | isTarball fn = TarPkg
                       | otherwise    = DirPkg

-- |Put the tarball for this package in the local repository
installTarball :: [GlobalFlag]
               -> LocalRepository -- ^Location of the local repository
               -> LocalSource -- ^What kind of package source
               -> PackageIdentifier
               -> FilePath -- ^Where the package is in the filesystem
               -> IO (Either String ())
installTarball flgs localRepo src pkgId fn =
    do createDirectoryIfMissing True $ localRepoPath localRepo </> repoDir pkgId
       case src of
         TarPkg -> do copyFile fn dest
                      return $ Right ()
         DirPkg -> do
                  res <- makeSDist
                  case res of
                    Left err -> return $ Left err
                    Right tarFn -> do
                             renameFile tarFn dest
                             return $ Right ()
    where
      dest = localRepoPath localRepo </> tarballName pkgId
      makeSDist = do
        Log.debug flgs $ "Running cabal sdist in " ++ fn
        bracket getCurrentDirectory setCurrentDirectory $ \_ -> do
                    setCurrentDirectory fn
                    cabalRes <- rawSystem "cabal" ["sdist"]
                    case cabalRes of
                      ExitSuccess ->
                          do here <- getCurrentDirectory
                             return $ Right $ here </> "dist" </>
                                    display pkgId <.> "tar" <.> "gz"
                      ExitFailure code ->
                          return $ Left $
                                  "cabal sdist failed with " ++ show code

-- |Extract the index information from the supplied path, either as a
-- tarball or as a local package directory
processLocalSource :: FilePath
                   -> IO (Either String ((LocalSource, PackageIdentifier), T.Entry))
processLocalSource fn = do
  let src = classifyLocalSource fn
  res <- case src of
           TarPkg -> processTarball fn
           DirPkg -> processDirectory fn
  case res of
    Left err -> return $ Left $ "Processing package from " ++ fn ++ ": " ++
                err
    Right (pkgId, c) -> do
             ent <- either fail return $ toIndexEntry pkgId c
             return $ Right ((src, pkgId), ent)

-- |Extract the index information from a tarball
processTarball :: FilePath
               -> IO (Either String (PackageIdentifier, L.ByteString))
processTarball fn =
    withFile fn ReadMode $ \h ->
        do ents <- T.read . Z.decompress <$> L.hGetContents h
           case extractCabalFile ents of
             Nothing -> return $ Left "No cabal file found"

             -- Force reading the cabal file before we exit withFile
             Just res -> forceBS (snd res) >> return (Right res)

-- |Extract the index information from a directory containing a cabal
-- file
processDirectory :: FilePath
                 -> IO (Either String (PackageIdentifier, L.ByteString))
processDirectory d = catchJust selectExpected go $ \e ->
                     return $ Left $ show e
    where
      selectExpected e = guard (expected e) >> return e

      expected e = any ($ e) [ isDoesNotExistError
                             ]

      go = do
        fns <- getDirectoryContents d
        case filter isCabalFile fns of
          [c] -> processCabalFile c
          []  -> return $ Left "No cabal file found"
          _   -> return $ Left "More than one cabal file present"

      processCabalFile c = do
        let fn = d </> c
        pkgId <- package . packageDescription <$>
                 readPackageDescription V.normal fn
        if PackageName (takeBaseName c) == pkgName pkgId
          then do
            cabalFile <- withFile fn ReadMode $
                         forcedBS <=< L.hGetContents
            return $ Right (pkgId, cabalFile)
          else
            return $ Left $ "Package name does not match cabal \ 
                            \file name: " ++ fn

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
      step ent Nothing = (,) <$> entPackageId ent <*> entBytes ent
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
