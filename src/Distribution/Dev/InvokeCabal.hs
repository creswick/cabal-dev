{-# LANGUAGE CPP #-}
module Distribution.Dev.InvokeCabal
    ( actions )
where

#ifndef MIN_VERSION_Cabal
#define MIN_VERSION_Cabal(a,b,c) 1
#endif

import Control.Arrow          ( left, right )
import Distribution.Verbosity ( Verbosity, showForCabal )
#if MIN_VERSION_Cabal(1,8,0)
import Distribution.Simple.Program   ( findProgramLocation )
#elif MIN_VERSION_Cabal(1,4,0)
import Distribution.Simple.Program   ( findProgramOnPath )
#else
#error Cabal version unsupported
#endif
import System.Console.GetOpt  ( OptDescr )
import System.Directory       ( canonicalizePath )
import System.Exit            ( ExitCode(..) )
import System.Cmd             ( rawSystem )

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

#if MIN_VERSION_Cabal(1,8,0)
#elif MIN_VERSION_Cabal(1,4,0)
findProgramLocation :: Verbosity -> FilePath -> IO (Maybe FilePath)
findProgramLocation = flip findProgramOnPath
#else
#error Cabal version unsupported
#endif

actions :: String -> CommandActions
actions act = CommandActions
              { cmdDesc = "Invoke cabal-install with the development configuration"
              , cmdRun = \flgs _ args -> invokeCabal flgs (act:args)
              , cmdOpts = [] :: [OptDescr ()]
              , cmdPassFlags = True
              }

invokeCabal :: [GlobalFlag] -> [String] -> IO CommandResult
invokeCabal flgs args = do
  either (return . CommandError) (invokeCabalCfg . (++ args)) =<< setup flgs

setup :: [GlobalFlag] -> IO (Either String [String])
setup flgs = do
  s <- initPkgDb =<< resolveSandbox flgs
  cfgIn <- getCabalConfig flgs
  let cfgOut = cabalConf s
  cfgRes <- rewriteCabalConfig cfgIn cfgOut s
  let qualifyError err =
          "Error processing cabal config file " ++ cfgIn ++ ": " ++ err
  args <- extraArgs (getVerbosity flgs) cfgOut (getVersion s)
  return $
         left qualifyError $
         right (const $ args) $
         cfgRes

extraArgs :: Verbosity -> FilePath -> PackageDbType -> IO [String]
extraArgs v cfg pdb =
    do pdbArgs <- getPdbArgs
       return $ [cfgFileArg, verbosityArg] ++ pdbArgs
    where
      cfgFileArg = "--config-file=" ++ cfg
      verbosityArg = "--verbose=" ++ showForCabal v
      getPdbArgs =
          case pdb of
            (GHC_6_8_Db loc) -> do
                     p <- findProgramLocation v "ghc-pkg-6.8-compat"
                     case p of
                       Nothing -> return []
                       Just compat ->
                           do absCompat <- canonicalizePath compat
                              return $ [ "--ghc-pkg-options=--with-ghc-pkg=" ++ loc
                                       , "--with-ghc-pkg=" ++ absCompat
                                       ]
            _ -> return []

invokeCabalCfg :: [String] -> IO CommandResult
invokeCabalCfg args = do
  res <- rawSystem "cabal" args
  return $ case res of
             ExitSuccess      -> CommandOk
             ExitFailure code -> CommandError $
                                 "cabal-install failed with " ++ show code
