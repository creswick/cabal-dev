import Control.Monad ( unless )
import Distribution.Simple
import Distribution.Simple.Program.Types ( Program(..), simpleProgram )
import Distribution.PackageDescription ( PackageDescription(..), executables,
                                         hsSourceDirs, exeName, buildInfo )
import Distribution.Simple.LocalBuildInfo ( LocalBuildInfo, buildDir )
import Distribution.Simple.Setup ( BuildFlags, buildVerbose )
import Distribution.Simple.Utils ( rawSystemExit, findProgramVersion)
import Distribution.Verbosity ( Verbosity )

import System.Directory ( doesDirectoryExist )
import System.FilePath ( (</>) )

main = defaultMainWithHooks $
       simpleUserHooks { hookedPrograms = [cabalInstallProgram]
                       }

cabalInstallProgram :: Program
cabalInstallProgram = (simpleProgram "cabal") {
  programFindVersion = findProgramVersion "--numeric-version" id
  }