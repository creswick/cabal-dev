{- Copyright (c) 2010 Galois, Inc. -}
{-|

Rewrite the paths of a cabal-install config file, canonicalizing them
relative to the current path, supporting limited tilde expansion
(tilde expansion for the current user only)

-}
module Distribution.Dev.RewriteCabalConfig
    ( rewriteCabalConfig
    )
where

import Data.Maybe                ( fromMaybe )
import Control.Monad             ( liftM )
import Distribution.Simple.Utils ( withUTF8FileContents, writeUTF8File )
import Distribution.ParseUtils   ( readFields, ParseResult(..), Field(..) )
import System.Directory          ( canonicalizePath, getHomeDirectory )
import Distribution.Dev.Sandbox  ( Sandbox, KnownVersion, pkgConf )
import Text.PrettyPrint.HughesPJ

-- |Rewrite a cabal-install config file so that all paths are made
-- absolute and canonical.
rewriteCabalConfig :: FilePath -- ^The input config file
                   -> FilePath -- ^The output config file
                   -> Sandbox KnownVersion
                   -> IO (Either String ())
rewriteCabalConfig cfgIn cfgOut s = do
  home <- getHomeDirectory
  rewriteConfig (expandCabalConfig home) (setPackageDb s) cfgIn cfgOut

-- |Given an expansion configuration, read the input config file and
-- write the expansion into the output config file
rewriteConfig :: Expand IO -> ([Field] -> [Field])
              -> FilePath -> FilePath -> IO (Either String ())
rewriteConfig expand proc srcConfig destConfig =
    withUTF8FileContents srcConfig $ \s ->
        case readFields s of
          ParseFailed err -> return $ Left $ show err
          ParseOk _ fs    ->
              fmap Right $
              writeUTF8File destConfig . show . ppTopLevel . proc =<<
              rewriteTopLevel expand fs

setPackageDb :: Sandbox KnownVersion -> [Field] -> [Field]
setPackageDb s = (F 0 "package-db" (pkgConf s):) . filter (not . isPackageDb)
    where
      isPackageDb (F _ "package-db" _) = True
      isPackageDb _                  = False

rewriteTopLevel :: Monad m => Expand m -> [Field] -> m [Field]
rewriteTopLevel = mapM . rewriteField

rewriteField :: Monad m => Expand m -> Field -> m Field
rewriteField expand field =
    case field of
      F l name val -> F l name `liftM` rewriteLeaf name val
      Section l name key fs -> Section l name key `liftM`
                               rewriteSection name fs
      _ -> error $ "Only top-level fields and sections \ 
                   \supported. Not: " ++ show field
    where
      rewriteLeaf name val
          | name `elem` eLeaves expand = eExpand expand val
          | otherwise                  = return val

      rewriteSection s = rewriteTopLevel $
                         fromMaybe don'tExpand $
                         lookup s $ eSections expand

--------------------------------------------------
-- Output formatting

ppField :: Field -> Doc
ppField (F _ k v) = text k <> colon <+> text v
ppField (Section _ k v fs) = (text k <+> text v) $+$
                             nest 2 (vcat $ map ppField fs)
ppField f = error $ "Pretty printing not implemented: " ++ show f

ppTopLevel :: [Field] -> Doc
ppTopLevel = vcat . map ppField

--------------------------------------------------
-- Expanding fields

data Expand m = Expand { eExpand :: String -> m String
                       , eLeaves :: [String]
                       , eSections :: [(String, Expand m)]
                       }

-- |Replace a tilde as an initial path segment with a path.
expandTilde :: FilePath -> String -> String
expandTilde home s = case break (== '/') s of
                       ("~", rest) -> home ++ rest
                       _           -> s

-- |Identity expansion
don'tExpand :: Monad m => Expand m
don'tExpand = Expand return [] []

-- This is the part that's specific to the cabal-install config file:
-- These are the parts of the config file that are paths into the
-- local filesystem. Ideally, we'd use the cabal-install Config module
-- and operate on the datatype instead of the raw config file, but
-- that's internal to the cabal-install config file, so we use this
-- ad-hoc approach instead.
--
-- If the cabal-install config file changes, or if this list is not
-- complete, this code will have to be updated.
expandCabalConfig :: FilePath -> Expand IO
expandCabalConfig home =
    Expand { eExpand = ePath
           , eLeaves = [ "remote-repo-cache"
                       , "local-repo"
                       , "with-compiler"
                       , "with-hc-pkg"
                       , "scratchdir"
                       , "package-db"
                       , "extra-include-dirs"
                       , "extra-lib-dirs"
                       , "doc-index-file"
                       , "root-cmd"
                       , "symlink-bindir"
                       , "build-summary"
                       , "build-log"
                       ]
           , eSections = [("install-dirs", expandInstallDirs)]
           }
    where
      expandInstallDirs =
          Expand { eExpand = ePath
                 , eLeaves =
                     [ "prefix"
                     , "bindir"
                     , "libdir"
                     , "libsubdir"
                     , "libexecdir"
                     , "datadir"
                     , "datasubdir"
                     , "docdir"
                     , "htmldir"
                     , "haddockdir"
                     ]
                 , eSections = []
                 }

      ePath = canonicalizePath . expandTilde home
