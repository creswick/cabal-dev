-- Bootstrap cabal-dev in a sandbox (not installing any dependencies in the
-- user or global package databases)
--
-- This script runs on GHC 6.8 - GHC 6.12.
--
-- Run in the root of this package with:
-- @
--  runhaskell -isrc bin/runtests.hs
-- @
import qualified Distribution.Dev.RewriteCabalConfig as R

import Control.Applicative ( (<$>) )
import Data.List ( isPrefixOf )
import Data.Version ( Version(..), showVersion, parseVersion )
import Data.Maybe ( fromMaybe, maybeToList )
import Data.Monoid ( Monoid(..) )
import Control.Monad ( unless, (<=<), mplus, when )
import Distribution.Simple.Utils ( rawSystemExit, rawSystemStdout )
import Distribution.Verbosity ( Verbosity, normal, verbose, showForCabal
                              , deafening, intToVerbosity )
import System.Directory ( getAppUserDataDirectory, canonicalizePath, doesFileExist
                        , doesDirectoryExist, getCurrentDirectory
                        , createDirectoryIfMissing )
import System.FilePath ( (</>) )
import System.Environment ( getArgs )
import System.Console.GetOpt  ( OptDescr(..), ArgOrder(..), ArgDescr(..)
                              , getOpt
                              )
import Text.ParserCombinators.ReadP ( readP_to_S )

-- We would use cabal-dev itself here, but we use something a little
-- more brittle in order to make this code work with the Cabal that
-- came with GHC >= 6.8 so that we can bootstrap an installation
-- environment without installing any dependencies in a location that
-- could pollute other builds.

-- TODO:
--  * Attempt to use cabal-dev if it's present at the start

main :: IO ()
main = do
  cfg <- either fail return . parseArgs =<< getArgs
  let vb = getVerbosity cfg

  -- Create a sandbox to hold the installation
  sandbox <- getSandbox
  createDirectoryIfMissing True sandbox

  -- Bootstrap the ghc-pkg database for any dependencies we need to
  -- install in the process of building cabal-dev
  (ghcPkgDb, ghcVer) <- initGhcPkgDb vb sandbox

  -- Build a config file that points to the sandbox installation
  -- locations
  cfgOut <- createCabalConfig sandbox ghcPkgDb

  -- Invoke cabal-install running in this sandbox
  cabalArgs <- getCabalArgs vb sandbox cfgOut ghcVer
  rawSystemExit vb "cabal" $
                cabalArgs ++ [ "--config-file=" ++ cfgOut
                             , "install"
                             , "--flags=build-tests"
                             , "--verbose=" ++ showForCabal vb
                             ]
  let bin = (sandbox </>) . ("bin" </>)
  when (cfgRunTests cfg) $
       rawSystemExit vb (bin "cabal-dev-test")
       [bin "cabal-dev", "--plain", "--jxml=test-results.xml"]

-- The absolute path to the sandbox directory
getSandbox :: IO FilePath
getSandbox = let path = "cabal-dev"
                 handler = do cwd <- getCurrentDirectory
                              return $ cwd </> path
             in (canonicalizePath path) `catch` \_->handler

---------------------------------------------------------------------
-- Identifying GHC version so that we know how to initialize and what
-- flags we need to supply to use the sandbox

-- We would ideally use Distribution.Dev.InitPkgDb here, but the Cabal
-- APIs that we are using changed between GHC releases, and since
-- we're not running under cabal (yet), it's not possible to use e.g.
-- CPP hacks to conditionally use them. We might want to introspect
-- the library to see how to call it as part of the build process, but
-- I expect that would be more work than re-implementing the minimal
-- functionality we need here.

-- |Initialize a GHC package database in the specified sandbox,
-- returning the package database path and the version of GHC that it
-- corresponds to.
initGhcPkgDb :: Verbosity -> FilePath -> IO (FilePath, Version)
initGhcPkgDb vb sbox = do
  ver <- identifyGhcPkg vb
  let (fn, doInit) = ghcPkgSettings ver
      loc = sbox </> fn

  -- Actually initialize the database
  doInit vb loc

  return (loc, ver)

-- |Identify the GHC version by calling ghc-pkg and parsing its output
identifyGhcPkg :: Verbosity -> IO Version
identifyGhcPkg vb =
    do verStr <- rawSystemStdout vb "ghc-pkg" ["--version"]
       case parseVer verStr of
         []  -> fail $ "Failed to identify GHC from: " ++ show verStr
         [v] -> return v
    where
      parseVer s = do
        suff        <- maybeToList $ dropPrefix "GHC package manager version " s
        (ver, "\n") <- readP_to_S parseVersion suff
        return ver

dropCommonPrefix ::  Eq a => [a] -> [a] -> ([a], [a])
dropCommonPrefix (x:xs) (y:ys) | x == y = dropCommonPrefix xs ys
dropCommonPrefix xs ys = (xs, ys)

dropPrefix :: Eq a => [a] -- ^ Prefix
           -> [a] -- ^ String to check
           -> Maybe [a] -- ^ If the prefix fully matched, then Just the suffix
dropPrefix pfx s = case dropCommonPrefix pfx s of
                     ([], suff) -> Just suff
                     _          -> Nothing

-- |Get the ghc-pkg filename and the appropriate procedure for
-- initializing a ghc package database for this version
--
-- XXX: this is duplicated in Distribution.Dev.InitPkgDb
ghcPkgSettings :: Version -> (FilePath, Verbosity -> FilePath -> IO ())
ghcPkgSettings v = ("packages-" ++ showVersion v ++ ".conf", pkgType)
    where
      pkgType | canUseGhcPkg v  = ghcPkgInit
              | isFileBased v = fileBased
              | otherwise = error $ "Don't know how to initialize the\ 
                                    \ package database for GHC " ++
                                    showVersion v

isFileBased :: Version -> Bool
isFileBased v = v >= mkVer [6,8] && v < mkVer [6,11]

canUseGhcPkg :: Version -> Bool
canUseGhcPkg v = v >= mkVer [6,12]

needsWrapper :: Version -> Bool
needsWrapper v = v >= mkVer [6,8] && v < mkVer [6, 9]

-- |Call ghc-pkg init (GHC >= 6.12)
ghcPkgInit :: Verbosity -> FilePath -> IO ()
ghcPkgInit vb pkgLoc = do
  extant <- doesDirectoryExist pkgLoc
  unless extant $ rawSystemExit vb "ghc-pkg" ["init", pkgLoc]

-- |Write an empty GHC package database for GHC >= 6.8 && < 6.12
fileBased :: Verbosity -> FilePath -> IO ()
fileBased _ pkgLoc = do
  extant <- doesFileExist pkgLoc
  unless extant $ writeFile pkgLoc "[]"

mkVer :: [Int] -> Version
mkVer xs = Version xs []

---------------------------------------------------------------
-- Invoking cabal with the correct arguments to use the sandbox

-- |Get the command-line arguments that we need to supply to
-- cabal-install in order to use the sandbox to download packages and
-- install depenencies
getCabalArgs :: Verbosity -> FilePath -> FilePath -> Version -> IO [String]
getCabalArgs vb sandbox cfgFile v = (cfgFileArg:) `fmap` extraArgs
    where
      cfgFileArg = "--config-file=" ++ cfgFile
      extraArgs = if needsWrapper v then v68Args else return []

      v68Args = do
        -- Build ghc-pkg-6_8-compat by itself. It has no build
        -- dependencies aside from base and Cabal > 1.2, so we can
        -- build it without installing any dependencies anywhere.
        rawSystemExit vb "cabal"
                          [ "--config-file=" ++ cfgFile
                          , "install", "--flags=no-cabal-dev" ]

        -- Now we can use ghc-pkg-6_8_compat when installing
        -- dependencies for the rest of cabal-dev (notably, a newer
        -- version of Cabal)
        let compat = sandbox </> "bin" </> "ghc-pkg-6_8-compat"

        -- We don't tell ghc-pkg-6_8-compat where the original ghc-pkg
        -- is. Just use the version on the PATH because that's how we
        -- found it in the first place.
        return [ "--with-ghc-pkg=" ++ compat ]

-- Rewrite the cabal-install config file to have absolute paths
createCabalConfig :: FilePath -> FilePath -> IO FilePath
createCabalConfig sandbox ghcPkgDb = do
  let cfgIn = "admin" </> "cabal-config.in"
  let cfgOut = sandbox </> "cabal.config"
  cabalHome <- getAppUserDataDirectory "cabal"
  either fail (writeFile cfgOut)
      =<< R.rewriteCabalConfig (R.Rewrite cabalHome sandbox ghcPkgDb)
      =<< readFile cfgIn
  return cfgOut

--------------------------------------------------
-- Argument processing

parseArgs :: [String] -> Either String Config
parseArgs args =
    case getOpt Permute opts args of
      (flgs, [] , []) -> case partitionEithers $ map toConfig flgs of
                           ([], cfgs) -> Right $ mconcat cfgs
                           (es, _  ) -> Left $ unlines es
      (_, args, []) -> Left "This program takes no arguments"
      (_, _   , es) -> Left $ unlines es
    where

data Config = Config { cfgVerbosity :: Maybe Verbosity
                     , cfgRunTests :: Bool
                     }

instance Monoid Config where
    mempty = Config Nothing False
    mappend (Config v1 r1) (Config v2 r2) = Config (v2 `mplus` v1) (r1 || r2)

getVerbosity :: Config -> Verbosity
getVerbosity = fromMaybe normal . cfgVerbosity

data Flag = Verbose (Maybe String) | RunTests

opts :: [OptDescr Flag]
opts = [ Option "v" ["verbose"] (OptArg Verbose "LEVEL")
         "Specify verbosity level"
       , Option "" ["run-tests"] (NoArg RunTests)
         "Run the test suite after bootstrapping"
       ]

-- This function is in base 4 but not base 3
partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = go id id
    where
      go ls rs es = case es of
                      []            -> (ls [], rs [])
                      (Left x:es')  -> go (ls . (x:)) rs es'
                      (Right x:es') -> go ls (rs . (x:)) es'

maybeReads :: Read a => String -> Maybe a
maybeReads s = case reads s of
                 [(i, [])] -> Just i
                 _         -> Nothing

toConfig :: Flag -> Either String Config
toConfig (Verbose arg) =
    case arg of
      Nothing -> ok verbose
      Just s  -> maybe badSpec ok $ readV s
          where
            badSpec = Left $ "Bad verbosity specification: " ++ show s
    where
      ok v = Right mempty { cfgVerbosity = Just v }
      readV = intToVerbosity <=< maybeReads
toConfig RunTests = Right mempty { cfgRunTests = True }
