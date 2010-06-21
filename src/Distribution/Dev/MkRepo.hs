module Distribution.Dev.MkRepo
    ( actions
    )
where

import Control.Applicative ( (<$>) )
import Control.Monad ( liftM, ap, guard, forM_ )
import System.FilePath ( splitExtension, splitDirectories, (<.>), (</>) )
import System.Console.GetOpt ( OptDescr(..) )
import System.IO ( withFile, IOMode(..) )
import Distribution.Package ( PackageName(..), PackageIdentifier(..) )
import Distribution.Text ( simpleParse, display )
import qualified Data.ByteString.Lazy as L
import qualified Codec.Compression.GZip as Z
import qualified Codec.Archive.Tar as T

import Distribution.Dev.Command ( CommandActions(..), CommandResult(..) )
import Distribution.Dev.Flags ( GlobalFlag )
import Distribution.Dev.LocalRepo ( resolveLocalRepo )

actions :: CommandActions
actions = CommandActions
            { cmdDesc = "Create a new repository from cabal package sources"
            , cmdRun = \flgs _ -> mkRepo flgs
            , cmdOpts = [] :: [OptDescr ()]
            }

mkRepo :: [GlobalFlag] -> [String] -> IO CommandResult
mkRepo _    [] = return $ CommandError "No local package locations supplied"
mkRepo flgs fns = do
  localRepo <- resolveLocalRepo flgs
  putStrLn $ "Making a cabal repo in " ++ localRepo ++ " out of " ++ show fns
  forM_ fns $ \fn ->
      let (fn1, ext1) = splitExtension fn
          (_, ext2) = splitExtension fn1
      in case (ext2, ext1) of
           (".tar", ".gz") ->
               withFile fn ReadMode $ \h ->
                   do ents <- T.read . Z.decompress <$> L.hGetContents h
                      case extractCabalFile ents of
                        Nothing -> error "No cabal file found"
                        Just (pkgId, cf) -> do
                            putStrLn $ indexName pkgId
                            error "got it!"
           _ -> error "Treat as a directory"
  return CommandOk

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
                   let (p, e) = splitExtension f
                   guard $ e == ".cabal" && (PackageName p) == pkgName i
                   return i
            _      -> Nothing

      entBytes ent = case T.entryContent ent of
                       T.NormalFile x _ -> return x
                       _ -> Nothing

-- |The path to the .cabal file in the 00-index.tar file
indexName :: PackageIdentifier -> FilePath
indexName pkgId = display (pkgName pkgId) </>
                  display (pkgVersion pkgId) </>
                  (display (pkgName pkgId) <.> "cabal")
