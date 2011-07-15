{-|

Invoke ghc-pkg with the appropriate arguments to run in the cabal-dev
sandbox.

-}
module Distribution.Dev.GhcPkg
    ( actions
    )
where

import Distribution.Dev.Command ( CommandActions(..), CommandResult(..) )
import System.Console.GetOpt ( OptDescr )
import Distribution.Dev.Flags ( Config, getVerbosity )
import Distribution.Dev.InitPkgDb ( initPkgDb )
import Distribution.Dev.Sandbox ( resolveSandbox, getVersion
                                , PackageDbType(..), pkgConf )
import Distribution.Simple.Program ( emptyProgramConfiguration
                                   , requireProgram
                                   , runProgram
                                   , ghcPkgProgram
                                   )

actions :: CommandActions
actions = CommandActions
            { cmdDesc      = "Invoke ghc-pkg on the package database in"
            , cmdRun       = \cfg _ args -> invokeGhcPkg cfg args
            , cmdOpts      = [] :: [OptDescr ()]
            , cmdPassFlags = True
            }

invokeGhcPkg :: Config -> [String] -> IO CommandResult
invokeGhcPkg cfg args = do
  let v = getVerbosity cfg
  s <- initPkgDb v =<< resolveSandbox cfg
  (ghcPkg, _) <- requireProgram v ghcPkgProgram emptyProgramConfiguration
  let extraArgs = case getVersion s of
                    GHC_6_8_Db _ -> id
                    _            -> ("--no-user-package-conf":)

  runProgram v ghcPkg $ extraArgs $ "--package-conf" : pkgConf s : args
  return CommandOk
