{-# LANGUAGE CPP #-}
module Distribution.Dev.InvokeCabal
    ( actions
    , setup
    , extraArgs
    , cabalProgram
    )
where

import Distribution.Verbosity ( Verbosity, showForCabal )
import Distribution.Simple.Program ( Program( programFindVersion
                                            , programFindLocation
                                            )
                                   , findProgramVersion
                                   , simpleProgram
                                   , runProgram
                                   , requireProgram
                                   , programLocation
                                   , locationPath
                                   , emptyProgramConfiguration
                                   )
import Distribution.Simple.Utils ( withUTF8FileContents, writeUTF8File )
import System.Console.GetOpt  ( OptDescr )

import Distribution.Dev.Command            ( CommandActions(..)
                                           , CommandResult(..)
                                           )
import Distribution.Dev.Flags              ( GlobalFlag, getCabalConfig
                                           , getVerbosity
                                           )
import Distribution.Dev.InitPkgDb          ( initPkgDb )
import qualified Distribution.Dev.RewriteCabalConfig as R
import Distribution.Dev.Sandbox            ( resolveSandbox
                                           , cabalConf
                                           , Sandbox
                                           , KnownVersion
                                           , PackageDbType(..)
                                           , getVersion
                                           , pkgConf
                                           , sandbox
                                           )
import System.Directory ( canonicalizePath, getAppUserDataDirectory )
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
  s <- initPkgDb v =<< resolveSandbox flgs
  res <- setup s flgs
  case res of
    Left err -> return $ CommandError err
    Right args' -> do
             invokeCabalCfg v $ args' ++ args
             return CommandOk

setup :: Sandbox KnownVersion-> [GlobalFlag] -> IO (Either String [String])
setup s flgs = do
  let v = getVerbosity flgs
  cfgIn <- getCabalConfig flgs
  let cfgOut = cabalConf s
--  home <- getHomeDirectory
  cabalHome <- getAppUserDataDirectory "cabal"
  withUTF8FileContents cfgIn $ \cIn ->
      do cfgRes <- R.rewriteCabalConfig (R.Rewrite cabalHome (sandbox s) (pkgConf s)) cIn
         case cfgRes of
           Left err -> return $ Left $
                       "Error processing cabal config file " ++ cfgIn ++ ": " ++ err
           Right cOut -> do
                       writeUTF8File cfgOut cOut
                       args <- extraArgs v cfgOut (getVersion s)
                       return $ Right args

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
                         requireProgram v ghcPkgCompatProgram emptyProgramConfiguration
                     return $ [ longArg "ghc-pkg-options" $ withGhcPkg loc
                              , withGhcPkg $ locationPath $
                                programLocation ghcPkgCompat
                              ]
            _ -> return []

ghcPkgCompatProgram :: Program
ghcPkgCompatProgram  = p { programFindLocation =
                           \v -> do
                             res <- programFindLocation p v
                             case res of
                               Nothing -> return Nothing
                               Just loc -> Just `fmap` canonicalizePath loc
                         }
    where
      p = simpleProgram "ghc-pkg-6_8-compat"

cabalProgram :: Program
cabalProgram =
    (simpleProgram "cabal") { programFindVersion =
                                  findProgramVersion "--numeric-version" id
                            }

invokeCabalCfg :: Verbosity -> [String] -> IO ()
invokeCabalCfg v args = do
  (cabal, _) <- requireProgram v cabalProgram emptyProgramConfiguration
  runProgram v cabal args
