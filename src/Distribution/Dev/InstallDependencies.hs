module Distribution.Dev.InstallDependencies
    ( actions )
where

import System.Console.GetOpt ( OptDescr(..) )
import Distribution.Dev.Command ( CommandActions(..), CommandResult(..) )
import Distribution.Dev.Flags ( Config, getVerbosity, cfgCabalInstall )
import Distribution.Dev.Sandbox ( resolveSandbox )
import Distribution.Dev.InitPkgDb ( initPkgDb )
import Distribution.Dev.InvokeCabal ( setup )
import qualified Distribution.Dev.CabalInstall as CI
import Distribution.Simple.Program ( requireProgram
                                   , getProgramOutput
                                   , runProgram
                                   )
import Distribution.Simple.Program.Db ( emptyProgramDb )
import Distribution.Simple.Utils ( notice, warn )

actions :: CommandActions
actions = CommandActions
              { cmdDesc = "Install the dependencies for this package"
              , cmdRun = \cfg _ args -> installDependencies cfg args
              , cmdOpts = [] :: [OptDescr ()]
              , cmdPassFlags = True
              }

installDependencies :: Config -> [String] -> IO CommandResult
installDependencies flgs pkgNames = do
  let v = getVerbosity flgs
  s <- initPkgDb v =<< resolveSandbox flgs
  (cabal, _) <- requireProgram v (CI.program $ cfgCabalInstall flgs) emptyProgramDb
  eFeatures <- CI.getFeatures v cabal
  setupRes <- setup s cabal flgs CI.Install
  case (setupRes, eFeatures) of
    (Left err, _) -> return $ CommandError err
    (_, Left err) -> return $ CommandError err
    (Right args, Right features) ->
      if CI.hasOnlyDependencies features
      then do notice v "You are using a version of cabal-install that has \
                       \the --only-dependencies flag to the install command.\
                       \ Invoking that instead..."
              runProgram v cabal $ concat [ args
                                          , ["install", "--only-dependencies"]
                                          , pkgNames
                                          ]
              return CommandOk
      else do
        warn v "Using a work-around to install dependencies for cabal-install\
               \ less than 0.10"
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