{- Copyright (c) 2010 Galois, Inc. -}
{-|

add-source command

Puts local source packages into a repository readable by cabal-install

-}
{-# LANGUAGE CPP #-}
module Distribution.Dev.AddSource
    ( actions
    )
where

#ifndef MIN_VERSION_Cabal
#define MIN_VERSION_Cabal(a,b,c) 1
#endif

import Control.Applicative                   ( (<$>), (<*>) )
import Control.Arrow                         ( right )
import Control.Exception                     ( bracket )
import Control.Monad                         ( guard, (<=<), forM_ )
import Control.Monad.Error                   ( runErrorT, throwError )
import Control.Monad.Trans                   ( liftIO )
import Data.List                             ( isPrefixOf )
import Distribution.PackageDescription       ( packageDescription, package )
import Distribution.Package                  ( PackageIdentifier(..) )
#if MIN_VERSION_Cabal(1,6,0)
import Distribution.PackageDescription.Parse ( readPackageDescription )
import Distribution.Package                  ( PackageName(..) )
#elif MIN_VERSION_Cabal(1,4,0)
import Distribution.PackageDescription       ( readPackageDescription )
#else
#error Unsupported Cabal version
#endif
import Distribution.Text                     ( simpleParse, display )
import Network.HTTP                          ( mkRequest, RequestMethod(GET)
                                             , rspBody, simpleHTTP
                                             )
import Network.URI                           ( parseURI, uriScheme, URI
                                             , uriPath )
import System.Cmd                            ( rawSystem )
import System.Console.GetOpt                 ( OptDescr(..) )
import System.Directory                      ( getDirectoryContents
                                             , renameFile, copyFile
                                             , getCurrentDirectory
                                             , setCurrentDirectory
                                             , createDirectoryIfMissing
                                             , getTemporaryDirectory
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

import Distribution.Dev.Command ( CommandActions(..), CommandResult(..) )
import Distribution.Dev.Flags   ( Config, getVerbosity )
import Distribution.Dev.Sandbox ( resolveSandbox, localRepoPath
                                , Sandbox, indexTar, indexTarBase
                                )

import Distribution.Simple.Utils ( debug, notice )

actions :: CommandActions
actions = CommandActions
            { cmdDesc = "Add packages to a local cabal install repository"
            , cmdRun = \cfg _ -> addSources cfg
            , cmdOpts = [] :: [OptDescr ()]
            , cmdPassFlags = False
            }

addSources :: Config -> [String] -> IO CommandResult
addSources _    [] = return $ CommandError "No package locations supplied"
addSources flgs fns = do
  sandbox <- resolveSandbox flgs
  let v = getVerbosity flgs
  debug v $ "Making a cabal repo in " ++ localRepoPath sandbox ++
            " out of " ++ show fns
  results <- mapM (processLocalSource v) fns
  let errs = [e | Left e <- results]
      srcs = [s | Right s <- results]
  if not $ null errs
    then return $ CommandError $ unlines $
             "Errors finding cabal files:":map (showString "  ") errs
    else do
      let (sources, newEntries) = unzip srcs
      res <- readExistingIndex sandbox
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
               forM_ sources $ \(src, pkgId) ->
                   installTarball flgs sandbox src pkgId

               -- and now that the tarballs are in place, write out the
               -- updated index
               writeIndex sandbox newIndex
               return CommandOk

-- |Atomically write an index tarball in the supplied directory
writeIndex :: Sandbox a -- ^The local repository path
           -> [T.Entry] -- ^The index entries
           -> IO ()
writeIndex sandbox ents =
    do newIndexName <- withTmpIndex $ \(fn, h) ->
                       L.hPut h (T.write ents) >> return fn
       renameFile newIndexName $ indexTar sandbox
    where
      pth = localRepoPath sandbox
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
readExistingIndex :: Sandbox a -> IO (Either String [T.Entry])
readExistingIndex sandbox =
    readIndexFile `catch` \e ->
        if isDoesNotExistError e
        then return $ Right []
        else ioError e
    where
      readIndexFile = withFile (indexTar sandbox) ReadMode
                      (forceEntries . T.read <=< L.hGetContents)
      forceEntries es =
        let step _ l@(Left _) = l
            step x (Right xs) = Right (x:xs)
            es' = T.foldEntries step (Right []) Left es
        in either (const 0) length es' `seq` return es'


-- |What kind of package source is this?
data LocalSource = DirPkg FilePath | TarPkg FilePath deriving Show

-- |Determine if this filename looks like a tarball (otherwise, it
-- assumes that it's a directory and treats it as such)
classifyLocalSource :: String -> Either URI LocalSource
classifyLocalSource fn =
    case parseURI fn of
      Just u | isHttpUri u && isTarGzUri u -> Left u
      _      | isTarball fn                -> Right $ TarPkg fn
             | otherwise                   -> Right $ DirPkg fn
    where
      isHttpUri = (`elem` ["http:", "https:"]) . uriScheme
      isTarGzUri = (reverse ".tar.gz" `isPrefixOf`) . reverse . uriPath

-- |Put the tarball for this package in the local repository
installTarball :: Config
               -> Sandbox a -- ^Location of the local repository
               -> LocalSource -- ^What kind of package source
               -> PackageIdentifier
               -> IO (Either String ())
installTarball flgs sandbox src pkgId =
    do createDirectoryIfMissing True $ localRepoPath sandbox </> repoDir pkgId
       case src of
         TarPkg fn -> do copyFile fn dest
                         return $ Right ()
         DirPkg fn -> do
                  res <- makeSDist fn
                  case res of
                    Left err -> return $ Left err
                    Right tarFn -> do
                             renameFile tarFn dest
                             return $ Right ()
    where
      dest = localRepoPath sandbox </> tarballName pkgId
      makeSDist fn = do
        debug (getVerbosity flgs) $ "Running cabal sdist in " ++ fn
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

downloadTarball :: URI -> IO (Either String FilePath)
downloadTarball u = do
  tmpLoc <- getTemporaryDirectory
  bracket (openTempFile tmpLoc "package-.tar.gz") (hClose . snd) $ \(fn, h) ->
      do httpRes <- simpleHTTP $ mkRequest GET u
         case httpRes of
           Left err -> return $ Left $ show err
           Right resp ->
               do L.hPut h $ rspBody resp
                  return $ Right fn

-- |Extract the index information from the supplied path, either as a
-- tarball or as a local package directory
processLocalSource :: V.Verbosity -> FilePath
                   -> IO (Either String ((LocalSource, PackageIdentifier), T.Entry))
processLocalSource v fn =
    runErrorT $ do
      let cls = classifyLocalSource fn
      src <- case cls of
               Left u -> do
                  liftIO $ notice v $ "Downloading " ++ show u
                  TarPkg `fmap` eitherErrorIO (downloadTarball u)
               Right s -> return s
      (pkgId, c) <- eitherErrorIO $
                    case src of
                      TarPkg x -> processTarball x
                      DirPkg x -> processDirectory v x
      ent <- eitherError $ toIndexEntry pkgId c
      return ((src, pkgId), ent)
    where
      eitherError = either throwError return
      eitherErrorIO = eitherError <=< liftIO

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

#if MIN_VERSION_Cabal(1,6,0)
mkPackageName :: String -> PackageName
mkPackageName = PackageName

displayPackageName :: PackageName -> String
displayPackageName = display
#elif MIN_VERSION_Cabal(1,4,0)
mkPackageName :: String -> String
mkPackageName = id

displayPackageName :: String -> String
displayPackageName = id
#else
#error Unsupported cabal version
#endif

-- |Extract the index information from a directory containing a cabal
-- file
processDirectory :: V.Verbosity -> FilePath
                 -> IO (Either String (PackageIdentifier, L.ByteString))
processDirectory v d = go `catch` \e ->
                     if expected e
                     then return $ Left $ show e
                     else ioError e
    where
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
        if mkPackageName (takeBaseName c) == pkgName pkgId
          then do
            notice v $ "Building source dist at " ++ d ++ " for " ++ display pkgId
            cabalFile <- withFile fn ReadMode $
                         forcedBS <=< L.hGetContents
            return $ Right (pkgId, cabalFile)
          else
            return $ Left $ unlines $
                       [ "Package name does not match cabal file name:"
                       , " filename = " ++ fn
                       , " package name 1 = " ++ show (mkPackageName (takeBaseName c))
                       , " package name 2 = " ++ show (pkgName pkgId)
                       ]

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
                   let cabalName = mkPackageName $ takeBaseName f
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
indexName pkgId = repoDir pkgId </>
                  (displayPackageName (pkgName pkgId) <.> "cabal")

-- |The path to the tarball in the local repository
tarballName :: PackageIdentifier -> FilePath
tarballName pkgId = repoDir pkgId </> (display pkgId <.> "tar" <.> "gz")

repoDir :: PackageIdentifier -> FilePath
repoDir pkgId = displayPackageName (pkgName pkgId) </>
                display (pkgVersion pkgId)
