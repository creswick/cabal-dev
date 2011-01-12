import qualified Codec.Archive.Tar as Tar
import qualified Data.ByteString.Lazy.Char8 as L
import Control.Applicative ( (<$>), (<*>), pure )
import Control.Monad ( replicateM, when )
import Control.Monad.Random ( Rand, getRandomR, evalRandIO )
import Data.Char ( isAscii, isAlphaNum, isLetter )
import Data.List ( intercalate )
import Data.Monoid ( mempty )
import Distribution.Package ( PackageIdentifier(..), PackageName(..)
                            , packageId, packageName )
import Distribution.Simple.GHC ( getInstalledPackages )
import Distribution.Simple.Compiler ( PackageDB(..)
                                    )
import Distribution.Simple.Program ( configureAllKnownPrograms
                                   , addKnownPrograms
                                   , emptyProgramConfiguration
                                   , ghcPkgProgram
                                   , ghcProgram
                                   , ProgramConfiguration
                                   )
import Distribution.Simple.PackageIndex ( lookupSourcePackageId, allPackages
                                        , SearchResult(..), searchByName
                                        , PackageIndex
                                        )
import Distribution.InstalledPackageInfo ( installedPackageId )
import Distribution.Simple.Utils ( withTempDirectory, info )
import Distribution.Text ( simpleParse, display )
import Distribution.Verbosity ( normal, Verbosity, showForCabal, verbose )
import Distribution.Version ( Version(..) )
import Distribution.Dev.Flags ( parseGlobalFlags )
import Distribution.Dev.InitPkgDb ( initPkgDb )
import Distribution.Dev.Sandbox ( newSandbox, pkgConf, Sandbox, KnownVersion, sandbox, indexTar )
import System.IO ( withBinaryFile, IOMode(ReadMode) )
import System.Cmd ( rawSystem )
import System.Process ( readProcessWithExitCode )
import System.Directory ( getTemporaryDirectory, createDirectory )
import System.Environment ( getArgs )
import System.Exit ( ExitCode(ExitSuccess) )
import System.FilePath ( (</>), (<.>), takeExtension )
import System.Random ( RandomGen )
import Test.Framework ( defaultMainWithArgs, Test, testGroup )
import Test.Framework.Providers.HUnit ( testCase )
import Test.HUnit ( (@?=) )
import qualified Test.HUnit as HUnit

import Distribution.Client.Config -- ( parseConfig )
import Distribution.Client.Setup ( GlobalFlags(..) )
import Distribution.Simple.InstallDirs
import Distribution.Simple.Setup
import Distribution.ParseUtils
import Data.Function ( on )


tests :: FilePath -> [Test]
tests p =
    [ testGroup "Basic invocation tests" $ testBasicInvocation p
    , testGroup "Sandboxed" $
      [ testCase "add-source stays sandboxed (no-space dir)" $
        addSourceStaysSandboxed normal p "fake-package."
      , testCase "add-source stays sandboxed (dir with spaces)" $
        addSourceStaysSandboxed normal p "fake package."
      , testCase "Builds ok regardless of the state of the logs directory" $
        assertLogLocationOk normal p
      , testCase "Index tar files contain all contents" $
        assertTarFileOk normal p
      ]
    , testGroup "Parsing and serializing" $
      [ testCase "simple round-trip test" $
        assertRoundTrip

      -- , testCase "sample echo test" $
      --   configEcho
      ]
    ]

testBasicInvocation :: FilePath -> [Test]
testBasicInvocation p =
    [ testCase "--help exists with success" $
      assertExitsSuccess p ["--help"]

    , testCase "--version exists with success" $
      assertExitsSuccess p ["--version"]

    , testCase "--numeric-version has parseable output" $
      assertProgramOutput checkNumericVersion p ["--numeric-version"]

    , testCase "exits with failure when no arguments are supplied" $
      assertExitsFailure p []

    , testCase "short flag is passed through to cabal" $
      assertShortFlags
    ]

main :: IO ()
main = do
  args <- getArgs

  -- The first argument is the cabal-dev executable. The remaining
  -- arguments are passed directly to test-framework
  let (cabalDev, testFrameworkArgs) =
          case args of
            (x:xs) -> (x, xs)
            []     -> ("cabal-dev", [])

  defaultMainWithArgs (tests cabalDev) testFrameworkArgs

-- |Test that parsing and serializing a cabal-install SavedConfig with
-- spaces in the paths preserves the spaces properly.
--
-- This tests only a select few of the fields and leaves the rest
-- empty. In practice, this has proven to be adequate.
--
-- This is really just testing that cabal-install is capable of
-- handling these fields. We have baked in knowledge of the syntax
-- that these fields need into RewriteCabalConfig. Ideally,
-- RewriteCabalConfig would read/write the data structure using the
-- code from cabal-install. We haven't done that because pulling in
-- all of the code from cabal-install into a production build is
-- pretty hackish. I think that cabal-dev ought to be a fork or
-- alternate build of cabal-install.
--
-- Note that this test is the whole reason that we have the custom
-- build type.
assertRoundTrip :: HUnit.Assertion
assertRoundTrip =
    do parsed <- case parseConfig mempty $ showConfig configWithSpaces of
                   ParseFailed err -> fail $ "Parse failed: " ++ show err
                   ParseOk _ val   -> return val

       let check msg f =
               (HUnit.assertEqual msg `on` f) parsed configWithSpaces

       check "globalLocalRepos" (globalLocalRepos . savedGlobalFlags)
       check "cache" (globalCacheDir . savedGlobalFlags)
       check "prefix" (show . prefix . savedUserInstallDirs)
       check "bindir" (show . bindir . savedUserInstallDirs)
       check "packageDB" (configPackageDB . savedConfigureFlags)
    where
      configWithSpaces =
          mempty { savedGlobalFlags =
                   mempty { globalLocalRepos = [pfx "repos"]
                          , globalCacheDir = toFlag $ pfx "cache"
                          }
                 , savedUserInstallDirs =
                   mempty { prefix = toFlag $ toPathTemplate $ pfx ""
                          , bindir = toFlag $ toPathTemplate $ pfx "binaries"
                          }
                 , savedConfigureFlags =
                   mempty { configPackageDB =
                            toFlag $ SpecificPackageDB $ pfx "package.db"
                          }
                 }

      pfx = ("/path with spaces" </>)


assertProgramOutput :: (String -> Bool) -> FilePath -> [String]
                    -> HUnit.Assertion
assertProgramOutput f = assertProgram "has proper output" $
                        \(c, s, _) -> (c == ExitSuccess) && f s

assertExitsSuccess :: FilePath -> [String] -> HUnit.Assertion
assertExitsSuccess = assertProgram "exits successfully" $
                     (ExitSuccess ==) . fst3

assertExitsFailure :: FilePath -> [String] -> HUnit.Assertion
assertExitsFailure = assertProgram "exits with failure" $
                     (ExitSuccess /=) . fst3


-- | Test for short flags.  Verifies that a short flag can make it past the cabal-dev
-- command line arg parser (and then, presumably, to cabal-install).
-- Created due to issue 6: https://github.com/creswick/cabal-dev/issues#issue/6
assertShortFlags :: HUnit.Assertion
assertShortFlags = (parseGlobalFlags $ words "install -f-curl") @?= ([],["install", "-f-curl"],[])

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

assertProgram :: String -> ((ExitCode, String, String) -> Bool) -> FilePath -> [String]
              -> HUnit.Assertion
assertProgram desc f progPath progArgs = do
  res <- readProcessWithExitCode progPath progArgs ""
  let msg = concat [ desc
                   , " failed for "
                   , progPath
                   , " "
                   , show progArgs
                   , ": "
                   , show res
                   ]
  HUnit.assertBool msg $ f res

-- |Check that cabal-dev add-source makes a package installable by
-- cabal-dev and that cabal-dev install for that package afterwards
-- makes it available in the sandbox, but not outside.
addSourceStaysSandboxed :: Verbosity -> FilePath -> FilePath
                        -> HUnit.Assertion
addSourceStaysSandboxed v cabalDev dirName =
    withTempPackage v dirName $ \readPackageIndex packageDir sb pId -> do
      let pkgStr = display pId
      sbPkgs <- readPackageIndex (privateStack sb)
      gPkgs <- readPackageIndex globalStack

      -- XXX: Cabal returns a single "empty" package if you try to
      -- read an empty package db, so we remove any package that has
      -- an empty name before comparing
      let s = filter ((/= "") . display) .
              map installedPackageId .
              allPackages

      HUnit.assertEqual
               "Newly created package index should be empty"
               (s gPkgs) (s sbPkgs)

      let withCabalDev f aa = do
            f cabalDev (["-s", sandbox sb] ++ aa)

      -- Fails because the package is not available. XXX: this
      -- could fail if our randomly chosen filename is available on
      -- hackage. We should actually use a cabal-install config
      -- with an empty package index
      withCabalDev assertExitsFailure ["install", pkgStr]

      withCabalDev assertExitsSuccess ["add-source", packageDir]

      -- Do the installation. Now this library should be registered
      -- with GHC
      withCabalDev assertExitsSuccess
                       ["install", pkgStr, "--verbose=" ++ showForCabal v]

      -- Check that we can find the library in our private package db stack
      pkgIdx' <- readPackageIndex (privateStack sb)
      info v $ dumpIndex pkgIdx'
      case lookupSourcePackageId pkgIdx' pId of
        []  -> HUnit.assertFailure $
               "After installation, package " ++ pkgStr ++ " was not found"
        [_] -> return ()
        _   -> HUnit.assertFailure $
               "Unexpectedly got more than one match for " ++ pkgStr

      -- And that we can't find it in the shared package db stack
      sharedIdx <- readPackageIndex sharedStack
      info v $ dumpIndex sharedIdx
      case lookupSourcePackageId sharedIdx pId of
        []  -> return ()
        _   -> HUnit.assertFailure $
               "Found " ++ pkgStr ++ " in a shared package db!"
    where
      -- The package dbs that cabal-dev will consult
      privateStack sb = [GlobalPackageDB, SpecificPackageDB $ pkgConf sb]

      globalStack = [GlobalPackageDB]

-- |Test that the existence or non-existence of the logs directory
-- does not cause build failures.
--
-- Before this test, we had a bug that derived from
-- System.Directory.canonicalizePath behaving differently depending on
-- the state of the filesystem. This test tickled the bug, and now we
-- are not using canonicalizePath.
assertLogLocationOk :: Verbosity -> FilePath -> HUnit.Assertion
assertLogLocationOk v cabalDev =
    mapM_ setupLogs
              [ -- Works fine when its not there
                (assertExitsSuccess, const $ return ())

              , -- Works fine when its a directory
                (assertExitsSuccess, createDirectory)

              , -- Fails if it's a file
                (assertExitsFailure, (`writeFile` "bogus log data"))
              ]
    where
      setupLogs (expectation, act) =
          withTempPackage v "check-build-log." $ \_ packageDir sb pId -> do
            let pkgStr = display pId
            let logsPth = sandbox sb </> "logs"
            let lsMsg msg =
                    when (v >= verbose) $ do
                      putStrLn msg
                      _ <- rawSystem "ls" ["-ld", logsPth]
                      return ()
            let withCabalDev f aa = do
                  f cabalDev (["-s", sandbox sb] ++ aa)

            _ <- act logsPth
            lsMsg "BEFORE"
            _ <- withCabalDev assertExitsSuccess ["add-source", packageDir]
            _ <- withCabalDev expectation ["install", pkgStr]
            lsMsg "AFTER"

assertTarFileOk :: Verbosity -> FilePath -> HUnit.Assertion
assertTarFileOk v cabalDev =
    withTempPackage v "check-tar-file." $ \_ packageDir sb pId -> do
      let withCabalDev f aa = do
            f cabalDev (["-s", sandbox sb] ++ aa)
      _ <- withCabalDev assertExitsSuccess ["add-source", packageDir]
      withBinaryFile (indexTar sb) ReadMode $ \h -> do
        entries <- Tar.read `fmap` L.hGetContents h
        let selectCabalFile _ m@(Just _) = m
            selectCabalFile e Nothing = do
              case Tar.entryContent e of
                Tar.NormalFile b _ | isCabalFile e -> Just b
                _                                  -> Nothing
            isCabalFile e = takeExtension (Tar.entryPath e) == ".cabal"
        let c = Tar.foldEntries selectCabalFile Nothing (const Nothing) entries
        case c of
          Nothing        -> HUnit.assertFailure "Failed to find a cabal file"
          Just extracted ->
              let baseCabalName = display (packageName pId) <.> "cabal"
              in  withBinaryFile (packageDir </> baseCabalName) ReadMode $ \h1 ->
                  do original <- L.hGetContents h1
                     HUnit.assertEqual "Cabal files before and after tarring"
                          original extracted
                     print (L.length original, L.length extracted)
                     L.putStr original

----------------------------------------------------------------
-- Utility code for testing sandboxing

-- Check that this string contains a numeric version
checkNumericVersion :: String -> Bool
checkNumericVersion s =
    case lines s of
      [l] -> case simpleParse l of
               Just v  -> let _ = v :: Version
                          in True
               Nothing -> False
      _   -> False

-- |Generate a minimal cabal file for a given package identifier
--
-- This Cabal file is useful for testing whether cabal-install can
-- install things, and what the side-effects of installing a package
-- are. Note that there are many side-effects that are not invoked by
-- this package, however!
genCabalFile :: PackageIdentifier -> String
genCabalFile pId =
    unlines
    [ "Name:                " ++ display (pkgName pId)
    , "Version:             " ++ display (pkgVersion pId)
    , "Build-type:          Simple"
    , "Cabal-version:       >=1.2"
    , "Maintainer: nobody"
    , "Description: This package contains no source code, and only enough in\
      \ the Cabal file to make it buildable"
    , "Category: Testing"
    , "License: BSD3"
    , "Synopsis: The most minimal package that will build"
    , "Library"
    , "  build-depends: base"
    ]

-- |Generate a random Cabal package identifier
--
-- XXX: If the package identifier is over a certain size, then
-- cabal-install will fail because a filename is "too long." It's
-- unclear what this limit is, but this function has been tweaked to
-- generate a shorter name. There is still a big enough range of
-- values that it should be pretty easy to find a name that doesn't
-- conflict with something else, though.
mkRandomPkgId :: RandomGen g => Rand g PackageIdentifier
mkRandomPkgId = PackageIdentifier <$> getRandomName <*> getRandomVersion
    where
      getRandomVersion = Version <$> getRandomBranch <*> pure tags
          where tags = []

      -- A branch is a version number
      getRandomBranch = do
       n <- getRandomR (1, 4)
       replicateM n $ getRandomR (0, 100)

      getRandomName = do
        i <- getRandomR (1, 5)
        segs <- replicateM i getRandomSegment
        return $ PackageName $ intercalate "-" segs

      -- A Cabal package name segment must be at least one character
      -- long, consist of alphanumeric characters, and contain at
      -- least one letter.
      getRandomSegment = do
        aChar <- getRandomChar pkgLetter
        before <- getRandomStr pkgChar 5
        after <- getRandomStr pkgChar 5
        return $ before ++ [aChar] ++ after

      -- Given a predicate, select a random character from the first
      -- 256 characters that satsifies that predicate. An exception
      -- will be raised if no character matches the predicate.
      getRandomChar p = do
        let rng = filter p ['\0'..'\255']
        i <- getRandomR (0, length rng - 1)
        return $ rng !! i

      -- Get a random string of characters that match the predicate p
      -- of length [0..n]
      getRandomStr p n = do
        i <- getRandomR (0, n)
        replicateM i $ getRandomChar p

      pkgLetter c = isAscii c && isLetter c

      pkgChar c = isAscii c && isAlphaNum c

-- A human-readable package index dump
dumpIndex :: PackageIndex -> String
dumpIndex pkgIdx = unlines $ "Package index contents:":
                   map (showString "  " . display . packageId)
                   (allPackages pkgIdx)

-- Return a program database containing the programs we need to
-- load a package index
configurePrerequisites :: Verbosity -> IO ProgramConfiguration
configurePrerequisites v =
    configureAllKnownPrograms v $
    addKnownPrograms toConfigure emptyProgramConfiguration
    where
      -- The programs that need to be configured in order to load a
      -- package index
      toConfigure = [ ghcPkgProgram
                    , ghcProgram
                    ]

-- A package db stack that includes only shared package databases
sharedStack :: [PackageDB]
sharedStack = [GlobalPackageDB, UserPackageDB]

-- Given a package index, generate a package name that is not in
-- that index. It is unlikely that a randomly generated name
-- will be in the package index, but we will try three times to
-- make it wildly unlikely.
findUnusedPackageId :: Verbosity -> PackageIndex -> IO PackageIdentifier
findUnusedPackageId v pkgIdx = go []
    where
      go ps = do
        when (length ps > 2) $ HUnit.assertFailure $
             showString "Failed to find an unused package id. Tried:" $
             intercalate " " $
             map display ps

        pId <- evalRandIO mkRandomPkgId
        case searchByName pkgIdx (display $ packageName pId) of
          None -> do info v $ "Found unused package " ++ display pId
                     return pId
          _    -> go (pId:ps)

-- |Create a dummy Cabal package in a temporary directory and invoke a
-- function with the state dependent on the temporary directory.
withTempPackage
    :: Verbosity -> String
    -> (([PackageDB] -> IO PackageIndex)
        -> FilePath
        -> Sandbox KnownVersion -> PackageIdentifier -> IO a)
    -> IO a
withTempPackage v dirName f =
    do pConf <- configurePrerequisites v
       let readPackageIndex stack = getInstalledPackages v stack pConf

       -- Get the shared package index and choose a package identifier
       -- that is not in it
       pkgIdx <- readPackageIndex sharedStack
       info v $ dumpIndex pkgIdx
       pId <- findUnusedPackageId v pkgIdx

       -- Create a temporary directory in which to build and install
       -- the bogus package
       tmp <- getTemporaryDirectory
       withTempDirectory v tmp dirName $ \d -> do
         let sandboxDir = d </> "cabal-dev"
         let packageDir = d </> "pkg"
         let cabalFileName = packageDir </> display (pkgName pId) <.> "cabal"
         createDirectory packageDir
         writeFile cabalFileName (genCabalFile pId)
         info v $ unlines [ "Generated cabal file:"
                          ,  genCabalFile pId
                          ]
         when (v >= verbose) $ rawSystem "ls" ["-l"] >> return ()

         sb <- initPkgDb v =<< newSandbox v sandboxDir
         f readPackageIndex packageDir sb pId
