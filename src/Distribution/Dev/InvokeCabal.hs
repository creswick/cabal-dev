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
import Distribution.ParseUtils   ( readFields, ParseResult(..), Field(..) )
import Distribution.Version ( Version(..) )
import System.Console.GetOpt  ( OptDescr )

import Distribution.Dev.Command            ( CommandActions(..)
                                           , CommandResult(..)
                                           )
import Distribution.Dev.Flags              ( Config, getCabalConfig
                                           , getVerbosity
                                           , getExtras
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

actions :: String -> CommandActions
actions act = CommandActions
              { cmdDesc = "Invoke cabal-install with the development configuration"
              , cmdRun = \flgs _ args -> invokeCabal flgs (act:args)
              , cmdOpts = [] :: [OptDescr ()]
              , cmdPassFlags = True
              }

invokeCabal :: Config -> [String] -> IO CommandResult
invokeCabal flgs args = do
  let v = getVerbosity flgs
  s <- initPkgDb v =<< resolveSandbox flgs
  cabal <- CI.findOnPath v
  res <- setup s cabal flgs
  case res of
    Left err -> return $ CommandError err
    Right args' -> do
             runProgram v cabal $ args' ++ args
             return CommandOk

cabalArgs :: ConfiguredProgram -> Config -> IO (Either String [String])
cabalArgs cabal flgs = do
  let v = getVerbosity flgs
  s <- initPkgDb v =<< resolveSandbox flgs
  setup s cabal flgs

setup :: Sandbox KnownVersion -> ConfiguredProgram -> Config ->
         IO (Either String [String])
setup s cabal flgs = do
  let v = getVerbosity flgs
      extraPath = getExtras flgs
  cfgIn <- getCabalConfig flgs
  cVer <- CI.getFeatures v cabal
  let cfgOut = cabalConf s
  case cVer of
    Left err -> return $ Left err
    Right features -> do
      cabalHome <- CI.configDir features
      extras <- getExtraFields extraPath 
      case extras of
        Left (err) | Just ep <- extraPath 
                    -> return $ Left $ "Error processing extra cabal config file " ++ ep ++ ": " ++ err 
        Right extraFields ->
          withUTF8FileContents cfgIn $ \cIn ->
              do 
                 let rew = R.Rewrite{R.homeDir    = cabalHome
                                    ,R.sandboxDir = sandbox s
                                    ,R.packageDb  = pkgConf s
                                    ,R.quoteInstallDirs = CI.needsQuotes features
                                    ,R.extraFields = extraFields}
                 cfgRes <- R.rewriteCabalConfig rew cIn
                 case cfgRes of
                   Left err -> return $ Left $
                      "Error processing cabal config file " ++ cfgIn ++ ": " ++ err
                   Right cOut -> do
                      writeUTF8File cfgOut cOut
                      args <- extraArgs v cfgOut (getVersion s)
                      return $ Right args

getExtraFields :: Maybe String -> IO (Either String [Field])
getExtraFields (Just extraIn) = 
      withUTF8FileContents extraIn $ \cIn ->
       case readFields cIn of
          ParseFailed err -> return . Left . show $ err
          ParseOk _ fs    -> return . Right $ fs 
    
getExtraFields Nothing   = return . Right $ []

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
            (GHC_6_8_Db loc) | needsGHC68Compat -> do
                     -- Make Cabal call the wrapper that removes the
                     -- bad argument to ghc-pkg 6.8
                     debug v $ "Using GHC 6.8 compatibility wrapper for Cabal shortcoming"
                     (ghcPkgCompat, _) <-
                         requireProgram v ghcPkgCompatProgram emptyProgramConfiguration
                     return $ [ longArg "ghc-pkg-options" $ withGhcPkg loc
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
