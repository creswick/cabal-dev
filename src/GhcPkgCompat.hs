{-

This program is a hack that munges the invocation of ghc-pkg for
ghc-6.8 by cabal-1.8 so that it does not alter the global or user
package databases.

It is intended to be invoked by cabal as instructed by cabal-dev.

The bug that this program works around is not present in the cabal-1.9
development branch, and the bug only affects ghc-6.8 or possibly
older. This program has been tested with GHC 6.8.3.

It builds with at least GHC 6.8-6.12 and Cabal 1.4-1.8, so that it's
available for use with other sandboxes.

-}
module Main
    ( main
    )
where

import Distribution.Verbosity ( silent )
import Distribution.Simple.Utils ( rawSystemExit )

import Control.Monad ( msum )
import Data.Maybe ( fromMaybe, isJust )
import System.Environment ( getArgs )
import System.IO ( withFile, IOMode(AppendMode), hPutStrLn )

dropCommonPrefix :: Eq a => [a] -> [a] -> ([a], [a])
dropCommonPrefix (a:as) (b:bs) | a == b = dropCommonPrefix as bs
dropCommonPrefix as bs = (as, bs)

matchPrefix :: Eq a => [a] -> [a] -> Maybe [a]
matchPrefix pfx s = case dropCommonPrefix pfx s of
                      ([], s') -> Just s'
                      _        -> Nothing

ghcPkgExe :: [String] -> FilePath
ghcPkgExe = fromMaybe "ghc-pkg" . msum . map ghcPkgArg

ghcPkgArg :: String -> Maybe FilePath
ghcPkgArg = matchPrefix "--with-ghc-pkg="

isGhcPkgArg :: String -> Bool
isGhcPkgArg = isJust . ghcPkgArg

isNoUserPackage :: String -> Bool
isNoUserPackage = (== "--no-user-package-conf")

isPackageConf :: String -> Bool
isPackageConf = isJust . matchPrefix "--package-conf="

shouldDrop :: String -> Bool
shouldDrop s = isGhcPkgArg s || isNoUserPackage s

fixPackageDbSpec :: [String] -> [String]
fixPackageDbSpec args
    | any isPackageConf args = filter (not . (`elem` ["--global", "--user"])) args
    | otherwise              = args

main :: IO ()
main = do
  args <- getArgs
  let ghcPkg = ghcPkgExe args
      ghcPkgArgs = fixPackageDbSpec $ filter (not . shouldDrop) args
      v = silent
  withFile "xxx" AppendMode $ \h -> hPutStrLn h $ show $ ghcPkg:ghcPkgArgs
  rawSystemExit v ghcPkg ghcPkgArgs
