{-# LANGUAGE CPP #-}
module Distribution.Dev.InvokeCabal
    ( actions
    , setup
    , cabalProgram
    )
where

import Control.Arrow          ( left, right )
import Distribution.Verbosity ( Verbosity, showForCabal )
import Distribution.Simple.Program ( Program(programFindVersion)
                                   , findProgramVersion
                                   , simpleProgram
                                   , runProgram
                                   , requireProgram
                                   , programLocation
                                   , locationPath
                                   )
import Distribution.Simple.Program.Db ( emptyProgramDb )
import System.Console.GetOpt  ( OptDescr )

import Distribution.Dev.Command            ( CommandActions(..)
                                           , CommandResult(..)
                                           )
import Distribution.Dev.Flags              ( GlobalFlag, getCabalConfig
                                           , getVerbosity
                                           )
import Distribution.Dev.InitPkgDb          ( initPkgDb )
import Distribution.Dev.RewriteCabalConfig ( rewriteCabalConfig )
import Distribution.Dev.Sandbox            ( resolveSandbox
                                           , cabalConf
                                           , PackageDbType(..)
                                           , getVersion
                                           )

actions :: String -> CommandActions
actions act = CommandActions
              { cmdDesc = "Invoke cabal-install with the development configuration"
              , cmdRun = \flgs _ args -> invokeCabal flgs (act:args)
              , cmdOpts = [] :: [OptDescr ()]
              , cmdPassFlags = True
              }

invokeCabal :: [GlobalFlag] -> [String] -> IO CommandResult
invokeCabal flgs args = do
  let v = getVerbosity flgs
  res <- setup flgs
  case res of
    Left err -> return $ CommandError err
    Right args' -> do
             invokeCabalCfg v $ args' ++ args
             return CommandOk

setup :: [GlobalFlag] -> IO (Either String [String])
setup flgs = do
  let v = getVerbosity flgs
  s <- initPkgDb v =<< resolveSandbox flgs
  cfgIn <- getCabalConfig flgs
  let cfgOut = cabalConf s
  cfgRes <- rewriteCabalConfig cfgIn cfgOut s
  let qualifyError err =
          "Error processing cabal config file " ++ cfgIn ++ ": " ++ err
  args <- extraArgs v cfgOut (getVersion s)
  return $
         left qualifyError $
         right (const $ args) $
         cfgRes

extraArgs :: Verbosity -> FilePath -> PackageDbType -> IO [String]
extraArgs v cfg pdb =
    do pdbArgs <- getPdbArgs
       return $ [cfgFileArg, verbosityArg] ++ pdbArgs
    where
      longArg s = showString "--" . showString s . ('=':)
      cfgFileArg = longArg "config-file" cfg
      verbosityArg = longArg "verbose" $ showForCabal v
      withGhcPkg = longArg "with-ghc-pkg"
      getPdbArgs =
          case pdb of
            -- Make Cabal call the wrapper that removes the bad
            -- argument to ghc-pkg 6.8
            --
            -- XXX: If cabal-install is using a version of Cabal that
            -- does not have this bug, it should not use the wrapper!
            (GHC_6_8_Db loc) -> do
                     (ghcPkgCompat, _) <-
                         requireProgram v ghcPkgCompatProgram emptyProgramDb
                     return $ [ longArg "ghc-pkg-options" $ withGhcPkg loc
                              , withGhcPkg $ locationPath $
                                programLocation ghcPkgCompat
                              ]
            _ -> return []

ghcPkgCompatProgram :: Program
ghcPkgCompatProgram  = simpleProgram "ghc-pkg-6_8-compat"

cabalProgram :: Program
cabalProgram =
    (simpleProgram "cabal") { programFindVersion =
                                  findProgramVersion "--numeric-version" id
                            }

invokeCabalCfg :: Verbosity -> [String] -> IO ()
invokeCabalCfg v args = do
  (cabal, _) <- requireProgram v cabalProgram emptyProgramDb
  runProgram v cabal args
