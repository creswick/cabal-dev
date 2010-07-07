module Distribution.Dev.InstallDependencies
    ( actions )
where

import System.Console.GetOpt ( OptDescr(..) )
import Distribution.Dev.Command ( CommandActions(..), CommandResult(..) )
import Distribution.Dev.Flags ( GlobalFlag, getVerbosity )
import Distribution.Dev.Sandbox ( resolveSandbox )
import Distribution.Dev.InitPkgDb ( initPkgDb )
import Distribution.Dev.InvokeCabal ( setup, cabalProgram )
import Distribution.Simple.Program ( requireProgram
                                   , getProgramOutput
                                   , runProgram
                                   )
import Distribution.Simple.Program.Db ( emptyProgramDb )
import Distribution.Simple.Utils ( notice )

actions :: CommandActions
actions = CommandActions
              { cmdDesc = "Install the dependencies for this package"
              , cmdRun = \flgs _ args -> installDependencies flgs args
              , cmdOpts = [] :: [OptDescr ()]
              , cmdPassFlags = True
              }

installDependencies :: [GlobalFlag] -> [String] -> IO CommandResult
installDependencies flgs pkgNames = do
  let v = getVerbosity flgs
  s <- initPkgDb v =<< resolveSandbox flgs
  setupRes <- setup s flgs
  case setupRes of
    Left err -> return $ CommandError err
    Right args ->
        do
          (cabal, _) <- requireProgram v cabalProgram emptyProgramDb
          out <- getProgramOutput v cabal $ concat
                 [ args
                 , ["install", "--dry-run", "--verbose=1"]
                 , pkgNames
                 ]
          let -- Drop the lines that say:
              --  Resolving dependencies...
              --  In order, the following [...]
              pkgIdStrs = drop 2 $ lines out

              -- We take the init if no pkgNames were supplied because
              -- that means we're working on the cabal file in the
              -- current directory.
              deps = case pkgNames of
                       [] | not $ null pkgIdStrs -> init pkgIdStrs
                       _  -> pkgIdStrs

          case deps of
            [] -> notice v "No dependencies need to be installed"
            _  -> do notice v $ "Installing dependencies: " ++ unwords deps
                     runProgram v cabal $ args ++ ("install":deps)

          return CommandOk