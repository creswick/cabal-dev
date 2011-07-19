{- Copyright (c) 2011 Galois, Inc. -}
{-|

Rewrite the paths of a cabal-install config file, canonicalizing them
relative to the current path, supporting limited tilde expansion
(tilde expansion for the current user only)

This module is written so that it will work out-of-the-box with GHC >=
6.8 && < 6.13 with no other packages installed.

-}
module Distribution.Dev.RewriteCabalConfig
    ( rewriteCabalConfig
    , Rewrite(..)
    , ppTopLevel
    , readConfigF
    , readConfigF_
    )
where

import Control.Applicative       ( Applicative, pure, (<$>) )
import Data.Maybe                ( fromMaybe )
import Data.Traversable          ( traverse, Traversable )
import Distribution.ParseUtils   ( Field(..), readFields, ParseResult(..) )
import Distribution.Simple.Utils ( readUTF8File )
import Text.PrettyPrint.HughesPJ

data Rewrite = Rewrite { homeDir          :: FilePath
                       , sandboxDir       :: FilePath
                       , packageDb        :: FilePath
                       , quoteInstallDirs :: Bool
                       }

readConfig :: String -> Either String [Field]
readConfig s = case readFields s of
                 ParseOk _ fs  -> Right fs
                 ParseFailed e -> Left $ show e

-- XXX: we should avoid this lazy IO that leaks a file handle.
readConfigF :: FilePath -> IO (Either String [Field])
readConfigF fn =
    (readConfig <$> readUTF8File fn) `catch` \e -> return $ Left $ show e

readConfigF_ :: FilePath -> IO [Field]
readConfigF_ fn = either error id <$> readConfigF fn

-- |Rewrite a cabal-install config file so that all paths are made
-- absolute and canonical.
rewriteCabalConfig :: Applicative f => Rewrite -> [Field] -> f [Field]
rewriteCabalConfig r = rewriteConfig expand (setPackageDb $ packageDb r)
  where
    expand = expandCabalConfig (quoteInstallDirs r) (homeDir r) (sandboxDir r)

-- |Given an expansion configuration, read the input config file and
-- write the expansion into the output config file
rewriteConfig :: Applicative f =>
                 Expand f -> ([Field] -> [Field]) -> [Field] -> f [Field]
rewriteConfig expand proc fs = proc <$> rewriteTopLevel expand fs

setPackageDb :: FilePath -> [Field] -> [Field]
setPackageDb pkgDb = (F 0 "package-db" pkgDb:) . filter (not . isPackageDb)
    where
      isPackageDb (F _ "package-db" _) = True
      isPackageDb _                  = False

rewriteTopLevel :: Applicative f => Expand f -> [Field] -> f [Field]
rewriteTopLevel = traverse . rewriteField

rewriteField :: Applicative m => Expand m -> Field -> m Field
rewriteField expand field =
    case field of
      F l name val -> F l name <$> rewriteLeaf name val
      Section l name key fs -> Section l name key <$>
                               rewriteSection name fs
      _ -> error $ "Only top-level fields and sections \ 
                   \supported. Not: " ++ show field
    where
      rewriteLeaf name val
          | name `elem` eLeaves expand = eExpand expand val
          | otherwise                  = pure val

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

data Expand f = Expand { eExpand :: String -> f String
                       , eLeaves :: [String]
                       , eSections :: [(String, Expand f)]
                       }

-- |Replace a tilde as an initial path segment with a path.
expandTilde :: FilePath -> String -> String
expandTilde home s = case break (== '/') s of
                       ("~", rest) -> home ++ rest
                       _           -> s

-- |Replace a tilde as an initial path segment with a path.
expandDot :: FilePath -> String -> String
expandDot sandbox s = case break (== '/') s of
                        (".", rest) -> sandbox ++ rest
                        _           -> s

-- |Identity expansion
don'tExpand :: Applicative f => Expand f
don'tExpand = Expand pure [] []

-- This is the part that's specific to the cabal-install config file:
-- These are the parts of the config file that are paths into the
-- local filesystem. Ideally, we'd use the cabal-install Config module
-- and operate on the datatype instead of the raw config file, but
-- that's internal to the cabal-install config file, so we use this
-- ad-hoc approach instead.
--
-- If the cabal-install config file changes, or if this list is not
-- complete, this code will have to be updated.
expandCabalConfig :: Applicative f =>
                     Bool -- ^Whether the install-dirs section of the
                          -- cabal config file will quote paths.
                          -- Versions of cabal-install prior to 0.9
                          -- required quoting. Versions 0.9 and later
                          -- forbit it.
                     -> FilePath -> FilePath -> Expand f
expandCabalConfig shouldQuote home sandbox =
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
          Expand { eExpand = quote . ePath
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

      -- How install-dirs should quote its paths
      quote | shouldQuote = fmap show
            | otherwise   = id

      ePath = pure . expandDot sandbox . expandTilde home
