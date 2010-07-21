import Control.Monad ( unless )
import Distribution.Simple
import Distribution.PackageDescription ( PackageDescription(..), executables,
                                         hsSourceDirs, exeName, buildInfo )
import Distribution.Simple.LocalBuildInfo ( LocalBuildInfo, buildDir )
import Distribution.Simple.Setup ( BuildFlags, buildVerbose )

import Distribution.Simple.Utils ( rawSystemExit )
import Distribution.Verbosity ( Verbosity )

import System.Directory ( doesDirectoryExist )
import System.FilePath ( (</>) )

main = defaultMainWithHooks $
       simpleUserHooks { buildHook = getCabalInstallSource }

getCabalInstallSource :: PackageDescription -> LocalBuildInfo -> UserHooks
                      -> BuildFlags -> IO ()
getCabalInstallSource pkgDesc lbi hooks flags =
  do -- cabal-install fails (exit 1) if the target unpack dir exists.
    exists <- doesDirectoryExist cabalInstallDir
    unless exists $ rawSystemExit v unpackCommand args
    buildHook simpleUserHooks updatedSrcDesc lbi hooks flags
  where v = buildVerbose flags
        unpackCommand = "cabal"
        srcDir        = buildDir lbi
        cabalInstallName  = "cabal-install-0.8.2"
        cabalInstallCabal = "cabal-install.cabal"
        cabalInstallDir   = srcDir </> cabalInstallName
        args            = ["unpack", "--dest="++srcDir, cabalInstallName]
        updatedSrcDesc  = addSrcDirs pkgDesc cabalInstallDir

addSrcDirs :: PackageDescription -> FilePath -> PackageDescription
addSrcDirs pkgDesc path =
  let notTargetTestExe exe = exeName exe /= "cabal-dev-test"
      updateExe exe | notTargetTestExe exe = id exe
                    | otherwise            = updateSrcDirs exe
      updateSrcDirs exe = exe {
          buildInfo = (buildInfo exe) {
             hsSourceDirs = (hsSourceDirs $ buildInfo exe) ++ [path] }}
  in pkgDesc { executables = map updateExe $ executables pkgDesc }