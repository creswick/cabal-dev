{-# LANGUAGE CPP #-}
module Distribution.Dev.InvokeCabal
    ( actions
    , setup
    , extraArgs
    , cabalArgs
    )
where

import Distribution.Verbosity ( Verbosity, showForCabal )
import Distribution.Simple.Program ( Program( programFindLocation )
                                   , ConfiguredProgram
                                   , emptyProgramConfiguration
                                   , locationPath
                                   , programLocation
                                   , requireProgram
                                   , runProgram
                                   , simpleProgram
                                   )
import Distribution.Simple.Utils ( withUTF8FileContents, writeUTF8File
                                 , debug, cabalVersion )
import Distribution.Version ( Version(..) )
import System.Console.GetOpt  ( OptDescr )

import Distribution.Dev.Command            ( CommandActions(..)
                                           , CommandResult(..)
                                           )
import Distribution.Dev.Flags              ( Config, getCabalConfig
                                           , getVerbosity
                                           )
import Distribution.Dev.InitPkgDb          ( initPkgDb )
import qualified Distribution.Dev.RewriteCabalConfig as R
import qualified Distribution.Dev.CabalInstall as CI
import Distribution.Dev.Sandbox            ( resolveSandbox
                                           , cabalConf
                                           , Sandbox
                                           , KnownVersion
                                           , PackageDbType(..)
                                           , getVersion
                                           , pkgConf
                                           , sandbox
                                           )
import Distribution.Dev.Utilities          ( ensureAbsolute )

actions :: CI.CabalCommand -> CommandActions
actions cc = CommandActions
              { cmdDesc = "Invoke cabal-install with the development configuration"
              , cmdRun = \flgs _ args -> invokeCabal flgs cc args
              , cmdOpts = [] :: [OptDescr ()]
              , cmdPassFlags = True
              }

invokeCabal :: Config -> CI.CabalCommand -> [String] -> IO CommandResult
invokeCabal flgs cc args = do
  let v = getVerbosity flgs
  s <- initPkgDb v =<< resolveSandbox flgs
  cabal <- CI.findOnPath v
  res <- setup s cabal flgs cc
  case res of
    Left err -> return $ CommandError err
    Right args' -> do
             runProgram v cabal $ args' ++ args
             return CommandOk

cabalArgs :: ConfiguredProgram -> Config -> CI.CabalCommand
          -> IO (Either String [String])
cabalArgs cabal flgs cc = do
  let v = getVerbosity flgs
  s <- initPkgDb v =<< resolveSandbox flgs
  setup s cabal flgs cc

setup :: Sandbox KnownVersion -> ConfiguredProgram -> Config ->
         CI.CabalCommand -> IO (Either String [String])
setup s cabal flgs cc = do
  let v = getVerbosity flgs
  cfgIn <- getCabalConfig flgs
  cVer <- CI.getFeatures v cabal
  let cfgOut = cabalConf s
  case cVer of
    Left err -> return $ Left err
    Right features -> do
      cabalHome <- CI.configDir features
      let rew = R.Rewrite cabalHome (sandbox s) (pkgConf s) (CI.needsQuotes features)
      withUTF8FileContents cfgIn $ \cIn ->
          do cfgRes <- R.rewriteCabalConfig rew cIn
             case cfgRes of
               Left err -> return $ Left $
                  "Error processing cabal config file " ++ cfgIn ++ ": " ++ err
               Right cOut -> do
                  writeUTF8File cfgOut cOut
                  (gOpts, cOpts) <- extraArgs v cfgOut (getVersion s)
                  let gFlags = map toArg gOpts
                      cFlags = map toArg $
                               filter (CI.supportsOption cc . fst) cOpts
                      args = concat [ gFlags, [CI.commandToString cc], cFlags ]
                  debug v $ "Complete arguments to cabal-install: " ++ show args
                  return $ Right args

toArg :: Option -> String
toArg (a, mb) = showString "--" .
                showString a $ maybe "" ('=':) mb

-- option name, value
type Option = (String, Maybe String)
type Options = [Option]

extraArgs :: Verbosity -> FilePath -> PackageDbType -> IO (Options, Options)
extraArgs v cfg pdb =
    do pdbArgs <- getPdbArgs
       return ([cfgFileArg], verbosityArg:pdbArgs)
    where
      longArg s = (,) s . Just
      cfgFileArg = longArg "config-file" cfg
      verbosityArg = longArg "verbose" $ showForCabal v
      withGhcPkg = longArg "with-ghc-pkg"
      getPdbArgs =
          case pdb of
            (GHC_6_8_Db loc) | needsGHC68Compat -> do
                     -- Make Cabal call the wrapper that removes the
                     -- bad argument to ghc-pkg 6.8
                     debug v $ "Using GHC 6.8 compatibility wrapper for Cabal shortcoming"
                     (ghcPkgCompat, _) <-
                         requireProgram v ghcPkgCompatProgram emptyProgramConfiguration
                     return $ [ longArg "ghc-pkg-options" $ toArg $ withGhcPkg loc
                              , withGhcPkg $ locationPath $
                                programLocation ghcPkgCompat
                              ]
            _ -> return []

-- XXX: this is very imprecise. Right now, we require a specific
-- version of Cabal, so this is ok (and is equivalent to True). Note
-- that this is the version of Cabal that THIS PROGRAM is being built
-- against, rather than the version that CABAL-INSTALL was built
-- against.
needsGHC68Compat :: Bool
needsGHC68Compat = cabalVersion < Version [1, 9] []

ghcPkgCompatProgram :: Program
ghcPkgCompatProgram  = p { programFindLocation =
                           \v -> do
                             res <- programFindLocation p v
                             case res of
                               Nothing -> return Nothing
                               Just loc -> Just `fmap` ensureAbsolute loc
                         }
    where
      p = simpleProgram "ghc-pkg-6_8-compat"
