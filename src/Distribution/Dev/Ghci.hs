module Distribution.Dev.Ghci
   ( actions )
where

import Distribution.Dev.Command ( CommandActions(..), CommandResult(..) )
import Distribution.Dev.Flags ( Config, getVerbosity, getSandbox )

import Distribution.Simple.Program ( Program( programFindVersion
                                            )
                                   , ConfiguredProgram( programDefaultArgs )
                                   , emptyProgramConfiguration
                                   , findProgramVersion
                                   , runProgram
                                   , requireProgram
                                   , simpleProgram
                                   , programVersion
                                   , programLocation
                                   , locationPath
                                   )
import Distribution.Simple.Utils ( debug )
import Distribution.Text ( display )
import System.Console.GetOpt  ( OptDescr )
import System.FilePath ((</>))

actions :: CommandActions
actions = CommandActions
              { cmdDesc = "Run ghci with the proper package database."
              , cmdRun = \cfg _ args -> invokeGhci cfg args
              , cmdOpts = [] :: [OptDescr ()]
              , cmdPassFlags = True
              }

packageConf :: String
packageConf = "packages-7.0.1.conf"

ghciArgs :: FilePath -> [String]
ghciArgs sandbox = [ "-package-conf", sandbox </> packageConf
                   , "-no-user-package-conf"
                   ]

configureGhci :: Config -> IO ConfiguredProgram
configureGhci cfg = do (ghci, _) <- requireProgram (getVerbosity cfg) ghciProgram emptyProgramConfiguration
                       return ghci { programDefaultArgs = ghciArgs (getSandbox cfg) }

-- XXX This invocation pattern is repeated in at least two places (see InvokeCabal)
invokeGhci :: Config -> [String] -> IO CommandResult
invokeGhci cfg args = do let v = getVerbosity cfg
                         ghci <- configureGhci cfg
                         debug v $ concat [ "Using ghci "
                                          , maybe "(unknown version)" display $ programVersion ghci
                                          , " at "
                                          , show (locationPath $ programLocation ghci)
                                          ]
                         runProgram v ghci args
                         return CommandOk

ghciProgram :: Program
ghciProgram = (simpleProgram "ghci") {
    programFindVersion = findProgramVersion "--numeric-version" id
  }
